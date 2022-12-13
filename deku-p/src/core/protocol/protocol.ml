open Receipt
open Deku_ledger
module Tezos_operation_hash = Deku_tezos.Tezos_operation_hash
open Deku_stdlib

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : Ocaml_wasm_vm.State.t;
    }

and t = protocol [@@deriving yojson]

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
      vm_state = Ocaml_wasm_vm.State.empty;
    }

let initial_with_vm_state ~vm_state =
  let (Protocol
        { included_operations; included_tezos_operations; ledger; vm_state = _ })
      =
    initial
  in
  Protocol { included_operations; included_tezos_operations; ledger; vm_state }

let apply_operation ~current_level protocol operation :
    (t * Receipt.t option * exn option) option =
  let open Operation.Initial in
  let (Protocol
        { included_operations; ledger; included_tezos_operations; vm_state }) =
    protocol
  in
  let (Initial_operation { hash; nonce = _; level; operation = content }) =
    operation
  in
  match
    (* TODO: check code through different lane *)
    (not (Included_operation_set.mem operation included_operations))
    && is_in_includable_window ~current_level ~operation_level:level
  with
  | true ->
      (* TODO: check that incorrect operations are removed from the pool *)
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let ledger, receipt, vm_state, error =
        match content with
        | Operation_ticket_transfer { sender; receiver; ticket_id; amount } -> (
            let receipt = Ticket_transfer_receipt { operation = hash } in
            match
              Ledger.transfer ~sender ~receiver ~ticket_id ~amount ledger
            with
            | Ok ledger -> (ledger, Some receipt, vm_state, None)
            | Error error -> (ledger, Some receipt, vm_state, Some error))
        | Operation_vm_transaction { sender; operation } as op -> (
            let receipt = Receipt.Vm_transaction_receipt { operation = hash } in
            let result () =
              let%ok ledger =
                if operation.tickets = [] then Ok ledger
                else
                  Ledger.with_ticket_table ledger (fun ~get_table ~set_table ->
                      let tickets = operation.tickets in
                      let result =
                        Ticket_table.take_tickets ~sender ~ticket_ids:tickets
                          (get_table ())
                      in
                      match result with
                      | Ok (_, table) -> Ok (set_table table)
                      | Error err -> (
                          match err with
                          | `Insufficient_funds -> Error "Insufficient_funds"))
              in
              let source = sender in
              let receipt = Vm_transaction_receipt { operation = hash } in
              match
                Ocaml_wasm_vm.(
                  Env.execute
                    (Env.make ~state:vm_state ~ledger ~sender ~source)
                    ~operation:operation.operation
                    ~tickets:
                      (List.map
                         (fun (k, v) -> (k, Deku_concepts.Amount.to_n v))
                         operation.tickets)
                    ~operation_hash:(Operation_hash.to_blake2b hash)
                  |> Env.finalize)
              with
              | Ok (vm_state, ledger) ->
                  Ok (ledger, Some receipt, vm_state, None)
              | Error err -> Error err
            in
            match result () with
            | Ok result -> result
            | Error err ->
                ( ledger,
                  Some receipt,
                  vm_state,
                  Some
                    (Failure
                       (Format.sprintf
                          "Error while executing external vm transaction : %s \
                           %s"
                          (Operation.show op) err)) ))
        | Operation_noop { sender = _ } -> (ledger, None, vm_state, None)
        | Operation_withdraw { sender; owner; amount; ticket_id } -> (
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
      Some
        ( Protocol
            { included_operations; included_tezos_operations; ledger; vm_state },
          receipt,
          error )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol
        { included_operations; included_tezos_operations; ledger; vm_state }) =
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
          { included_operations; included_tezos_operations; ledger; vm_state }
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
                  vm_state;
                })
        protocol operations
  | false -> protocol

let apply_tezos_operations tezos_operations protocol =
  List.fold_left apply_tezos_operation protocol tezos_operations

let parse_operation operation =
  match
    let (Signed_operation { key = _; signature = _; initial }) =
      Data_encoding.Binary.of_string_exn Operation.Signed.encoding operation
    in
    initial
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
        { included_operations; included_tezos_operations; ledger; vm_state }) =
    protocol
  in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol { included_operations; included_tezos_operations; ledger; vm_state }

let prepare ~parallel ~payload = parallel parse_operation payload

let apply ~current_level ~payload ~tezos_operations protocol =
  let protocol, receipts, errors =
    apply_payload ~current_level ~payload protocol
  in
  let protocol = clean ~current_level protocol in
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  Deku_metrics.set_latest_tx_count (List.length payload);
  (protocol, receipts, errors)
