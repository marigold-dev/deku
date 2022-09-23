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
    }

and t = protocol [@@deriving yojson]

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
      vm_state = External_vm_protocol.State.empty;
      receipts = Operation_hash.Map.empty;
    }

let initial_with_vm_state ~vm_state =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          receipts;
          vm_state = _;
        }) =
    initial
  in
  Protocol
    {
      included_operations;
      included_tezos_operations;
      ledger;
      receipts;
      vm_state;
    }

let apply_operation ~current_level protocol operation =
  let open Operation in
  let (Protocol
        {
          included_operations;
          ledger;
          included_tezos_operations;
          vm_state;
          receipts;
        }) =
    protocol
  in
  let (Operation
        { key = _; signature = _; hash; level; nonce = _; source; content }) =
    operation
  in
  match
    (* TODO: check code through different lane *)
    (not (Included_operation_set.mem operation included_operations))
    && Operation.is_in_includable_window ~current_level ~operation_level:level
  with
  | true ->
      (* TODO: check that incorrect operations are removed from the pool *)
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let ledger, new_receipts =
        match content with
        | Operation_ticket_transfer { receiver; ticket_id; amount } -> (
            let sender = source in
            match
              Ledger.transfer ~sender ~receiver ~ticket_id ~amount ledger
            with
            | Ok ledger -> (ledger, [])
            | Error _ -> (ledger, []))
        | Operation_vm_transaction _ -> assert false
        | Operation_noop ->
            Unix.sleepf 1.;
            (ledger, [])
        | Operation_withdraw { owner; amount; ticket_id } -> (
            let sender = source in
            match
              Ledger.withdraw ~sender ~destination:owner ~amount ~ticket_id
                ledger
            with
            | Ok (ledger, handle) -> (ledger, [ Withdraw_receipt handle ])
            | Error _ -> (ledger, []))
      in
      (* Add a receipt for all operations to remove them from the pool *)
      let new_receipts =
        Transaction_receipt { operation = hash } :: new_receipts
      in
      let receipts =
        List.fold_left
          (fun receipts receipt -> Operation_hash.Map.add hash receipt receipts)
          receipts new_receipts
      in
      Some
        ( Protocol
            {
              included_operations;
              included_tezos_operations;
              ledger;
              receipts;
              vm_state;
            },
          new_receipts )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          receipts;
          vm_state;
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
    (fun (protocol, rev_receipts) operation ->
      match apply_operation ~current_level protocol operation with
      | Some (protocol, receipts) -> (protocol, receipts @ rev_receipts)
      | None -> (protocol, rev_receipts)
      | exception _exn -> (* TODO: print exception *) (protocol, rev_receipts))
    (protocol, []) payload

let clean ~current_level protocol =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          receipts;
          vm_state;
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
    }

let find_withdraw_proof ~operation_hash protocol =
  let (Protocol { receipts; ledger; _ }) = protocol in
  match Operation_hash.Map.find_opt operation_hash receipts with
  | None -> Error `Unknown_operation
  | Some (Withdraw_receipt handle) ->
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
  let protocol, receipts = apply_payload ~current_level ~payload protocol in
  let protocol = clean ~current_level protocol in
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  (protocol, receipts)
