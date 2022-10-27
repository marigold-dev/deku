open Receipt
open Deku_ledger
open Deku_external_vm
module Tezos_operation_hash = Deku_tezos.Tezos_operation_hash
open Deku_stdlib

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : External_vm_protocol.State.t;
    }

and t = protocol [@@deriving yojson]

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
      vm_state = External_vm_protocol.State.empty;
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
        | Operation_vm_transaction { sender; operation; tickets } -> (
            let receipt = Receipt.Vm_transaction_receipt { operation = hash } in
            let result () =
              let%some ledger =
                if tickets = [] then Some ledger
                else
                  Ledger.with_ticket_table ledger (fun ~get_table ~set_table ->
                      let tickets =
                        List.map
                          (fun (x, y) -> (x, Deku_concepts.Amount.of_n y))
                          tickets
                      in
                      let%some _, table =
                        Ticket_table.take_tickets ~sender ~ticket_ids:tickets
                          (get_table ())
                        |> Result.to_option
                      in
                      Some (set_table table))
              in
              let ledger_state =
                object
                  val mutable ledger_state = ledger
                  method get_ledger = ledger_state

                  method take_tickets address =
                    Ledger.with_ticket_table ledger
                      (fun ~get_table ~set_table ->
                        let tickets, table =
                          Ticket_table.take_all_tickets (get_table ())
                            ~sender:address
                        in
                        ledger_state <- set_table table;
                        List.map
                          (fun (ticket_id, amount) ->
                            (ticket_id, Deku_concepts.Amount.to_n amount))
                          (List.of_seq tickets))

                  method deposit addr (ticket_id, amount) =
                    let amount = Deku_concepts.Amount.of_n amount in
                    ledger_state <- Ledger.deposit addr amount ticket_id ledger
                end
              in
              let%some source = Address.to_key_hash sender in
              let receipt = Vm_transaction_receipt { operation = hash } in
              match
                ( External_vm_client.apply_vm_operation_exn ~state:vm_state
                    ~level ~source ~tickets ~ledger_api:ledger_state
                    (Some (Operation_hash.to_blake2b hash, operation)),
                  ledger_state#get_ledger )
              with
              | vm_state, ledger -> Some (ledger, Some receipt, vm_state, None)
              | exception External_vm_client.Vm_execution_error error ->
                  let () = failwith error in
                  Some
                    ( ledger,
                      Some receipt,
                      vm_state,
                      Some (External_vm_client.Vm_execution_error error) )
            in
            match result () with
            | Some result -> result
            | None ->
                ( ledger,
                  Some receipt,
                  vm_state,
                  Some (Failure "Error while executing external vm transaction")
                ))
        | Operation_noop { sender } -> (
            match Address.to_key_hash sender with
            | Some source ->
                let vm_state =
                  External_vm_client.apply_vm_operation_exn
                    ~ledger_api:
                      (object
                         method take_tickets _ = assert false
                         method deposit _ _ = assert false
                      end)
                    ~level ~state:vm_state ~source ~tickets:[] None
                in
                (ledger, None, vm_state, None)
            | None ->
                (* This case should not be possible unless a node is byzantine. *)
                (ledger, None, vm_state, None))
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
