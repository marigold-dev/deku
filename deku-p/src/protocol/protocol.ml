open Receipt
open Deku_external_vm
module Tezos_operation_hash = Deku_tezos.Tezos_operation_hash
open Deku_stdlib

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : External_vm_protocol.State.t;
      receipts : Receipt.t Operation_hash.Map.t;
          (** Receipts of the included operations; also contains withdrawal receipts, which are used to
          generate withdrawal proofs. *)
      chain_id : Deku_tezos.Address.t;
    }

and t = protocol [@@deriving yojson]

let initial ~chain_id =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
      vm_state = External_vm_protocol.State.empty;
      receipts = Operation_hash.Map.empty;
      chain_id;
    }

let initial_with_vm_state ~vm_state ~chain_id =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          receipts;
          vm_state = _;
          chain_id;
        }) =
    initial ~chain_id
  in
  Protocol
    {
      included_operations;
      included_tezos_operations;
      ledger;
      receipts;
      vm_state;
      chain_id;
    }

let apply_operation ~current_level protocol operation :
    (t * Receipt.t option * exn option) option =
  let open Operation in
  let (Protocol
        {
          included_operations;
          ledger;
          included_tezos_operations;
          vm_state;
          receipts;
          chain_id = protocol_id;
        }) =
    protocol
  in
  let (Operation
        {
          key = _;
          signature = _;
          hash;
          level;
          nonce = _;
          source;
          content;
          chain_id;
        }) =
    operation
  in
  match
    (* TODO: check code through different lane *)
    (not (Included_operation_set.mem operation included_operations))
    && Operation.is_in_includable_window ~current_level ~operation_level:level
    && Deku_tezos.Address.equal chain_id protocol_id
  with
  | true ->
      (* TODO: check that incorrect operations are removed from the pool *)
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let ledger, receipt, vm_state, error =
        match content with
        | Operation_ticket_transfer { receiver; ticket_id; amount } -> (
            let sender = source in
            let receipt = Ticket_transfer_receipt { operation = hash } in
            match
              Ledger.transfer ~sender ~receiver ~ticket_id ~amount ledger
            with
            | Ok ledger -> (ledger, Some receipt, vm_state, None)
            | Error error -> (ledger, Some receipt, vm_state, Some error))
        | Operation_vm_transaction { operation; tickets } -> (
            let tickets =
              List.map
                (fun (ticket, amount) ->
                  (Ticket_id.to_tezos_ticket ticket, amount))
                tickets
            in
            let receipt = Vm_transaction_receipt { operation = hash } in
            match
              External_vm_client.apply_vm_operation_exn ~state:vm_state
                ~source:(Address.to_key_hash source)
                ~tickets
                (Some (Operation_hash.to_blake2b hash, operation))
            with
            | vm_state -> (ledger, Some receipt, vm_state, None)
            | exception External_vm_client.Vm_execution_error error ->
                ( ledger,
                  Some receipt,
                  vm_state,
                  Some (External_vm_client.Vm_execution_error error) ))
        | Operation_noop ->
            let vm_state =
              External_vm_client.apply_vm_operation_exn ~state:vm_state
                ~source:(Address.to_key_hash source)
                ~tickets:[] None
            in
            (ledger, None, vm_state, None)
        | Operation_withdraw { owner; amount; ticket_id } -> (
            let sender = source in
            match
              Ledger.withdraw ~sender ~destination:owner ~amount ~ticket_id
                ledger
            with
            | Ok (ledger, handle) ->
                ( ledger,
                  Some (Withdraw_receipt { handle; operation = hash }),
                  vm_state,
                  None )
            | Error error -> (ledger, None, vm_state, Some error))
      in
      let receipts =
        match receipt with
        | Some receipt -> Operation_hash.Map.add hash receipt receipts
        | None -> receipts
      in
      Some
        ( Protocol
            {
              included_operations;
              included_tezos_operations;
              ledger;
              receipts;
              vm_state;
              chain_id;
            },
          receipt,
          error )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          receipts;
          vm_state;
          chain_id;
        }) =
    protocol
  in
  let Tezos_operation.{ hash; operations } = tezos_operation in
  match
    not (Deku_tezos.Tezos_operation_hash.Set.mem hash included_tezos_operations)
  with
  | true ->
      let included_tezos_operations =
        Deku_tezos.Tezos_operation_hash.Set.add hash included_tezos_operations
      in
      let protocol =
        Protocol
          {
            included_operations;
            included_tezos_operations;
            ledger;
            receipts;
            vm_state;
            chain_id;
          }
      in
      List.fold_left
        (fun protocol tezos_operation ->
          match tezos_operation with
          | Tezos_operation.Deposit { destination; amount; ticket } ->
              let (Protocol
                    {
                      ledger;
                      included_operations;
                      included_tezos_operations;
                      receipts;
                      vm_state;
                      chain_id;
                    }) =
                protocol
              in
              let ticket_id =
                Ticket_id.from_tezos_ticket ticket |> Result.get_ok
              in
              let destination = Address.of_key_hash destination in
              let ledger = Ledger.deposit destination amount ticket_id ledger in
              Protocol
                {
                  ledger;
                  included_operations;
                  included_tezos_operations;
                  receipts;
                  vm_state;
                  chain_id;
                })
        protocol operations
  | false -> protocol

let apply_tezos_operations tezos_operations protocol =
  List.fold_left apply_tezos_operation protocol tezos_operations

let parse_operation operation =
  match
    let json = Yojson.Safe.from_string operation in
    Operation.t_of_yojson json
  with
  | operation -> Some operation
  | exception _exn -> (* TODO: print exception *) None

let apply_payload ~current_level ~payload protocol =
  List.fold_left
    (fun (protocol, rev_receipts, errors) operation ->
      match apply_operation ~current_level protocol operation with
      | Some (protocol, receipt, error) ->
          let rev_receipts =
            match receipt with
            | Some receipt -> receipt :: rev_receipts
            | None -> rev_receipts
          in
          let errors =
            match error with Some error -> error :: errors | None -> errors
          in
          (protocol, rev_receipts, errors)
      | None -> (protocol, rev_receipts, errors)
      | exception exn -> (protocol, rev_receipts, exn :: errors))
    (protocol, [], []) payload

let clean ~current_level protocol =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          receipts;
          vm_state;
          chain_id;
        }) =
    protocol
  in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol
    {
      included_operations;
      included_tezos_operations;
      ledger;
      receipts;
      vm_state;
      chain_id;
    }

let find_withdraw_proof ~operation_hash protocol =
  let (Protocol { receipts; ledger; _ }) = protocol in
  match Operation_hash.Map.find_opt operation_hash receipts with
  | None -> Error `Unknown_operation
  | Some (Withdraw_receipt { operation = _; handle }) ->
      let withdrawal_handles_hash =
        Ledger.withdrawal_handles_root_hash ledger
      in
      Ok
        ( handle,
          Ledger.withdrawal_handles_find_proof handle ledger,
          withdrawal_handles_hash )
  | _ ->
      (* FIXME? fragile *)
      prerr_endline "Found a receipt that does not match";
      Error `Unknown_operation

let prepare ~parallel ~payload = parallel parse_operation payload

let apply ~current_level ~payload ~tezos_operations protocol =
  (* FIXME: This is a temporary hack until we understand what's happening in
     the betanet. *)
  let protocol, receipts, errors =
    apply_payload ~current_level ~payload protocol
  in
  let protocol = clean ~current_level protocol in
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  (protocol, receipts, errors)
