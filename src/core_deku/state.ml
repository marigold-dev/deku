open Helpers
open Crypto
open Contracts
open Core

type t = {
  ledger : Ledger.t;
  contract_storage : Contract_storage.t;
}
[@@deriving yojson]

type contract_invocation_changes = {
  remaining_tickets : (Ticket_id.t * Amount.t) list;
  additional_operations : additional_operation list;
  new_storage : bytes;
}

and additional_operation = {
  user_operation : User_operation.t;
  operation_receipt : receipt option;
}

and contract_origination_changes = {
  originated_contract_address : Contract_address.t;
  initial_tickets : (Ticket_id.t * Amount.t) list;
}

and receipt =
  | Receipt_tezos_withdraw       of Ledger.Withdrawal_handle.t
  | Receipt_contract_origination of {
      sender : Address.t;
      outcome : [`Success of contract_origination_changes | `Failure];
    }
  | Receipt_contract_invocation  of {
      sender : Address.t;
      outcome : [`Success of contract_invocation_changes | `Failure];
    }
[@@deriving yojson]

let empty = { ledger = Ledger.empty; contract_storage = Contract_storage.empty }

let ledger t = t.ledger

let contract_storage t = t.contract_storage

let hash t = to_yojson t |> Yojson.Safe.to_string |> BLAKE2B.hash

let apply_tezos_operation t tezos_operation =
  let open Tezos_operation in
  let apply_internal_operation t internal_operation =
    let { ledger; contract_storage } = t in
    match internal_operation with
    | Tezos_deposit { destination; amount; ticket } ->
      let ticket =
        Ticket_id.of_tezos ticket
        |> Result.map_error ~f:(Fun.const "impossible to fail")
        |> Result.ok_or_failwith
        (* TODO: verify that its not possible to fail *) in
      let ledger =
        Ledger.deposit (Address.of_key_hash destination) amount ticket ledger
      in
      { ledger; contract_storage } in
  let { hash = _; payload } = tezos_operation in
  let { tezos_operation_hash = _; internal_operations } = payload in
  List.fold_left ~f:apply_internal_operation ~init:t internal_operations

let rec apply_user_operation ?sender t operation_hash user_operation =
  let open User_operation in
  let { source; initial_operation; hash = _ } = user_operation in
  let sender = sender |> Option.value ~default:(Address.of_key_hash source) in
  let { ledger; contract_storage } = t in
  match initial_operation with
  | Transaction { destination; amount; ticket } ->
    let%ok ledger =
      Ledger.transfer
        ~sender:(Address.of_key_hash source)
        ~destination amount ticket ledger in
    Ok ({ contract_storage; ledger }, None)
  | Tezos_withdraw { owner; amount; ticket } when Ticket_id.is_tezos ticket ->
    let%ok ticket =
      Ticket_id.to_tezos ticket
      |> Result.map_error ~f:(Fun.const `Insufficient_funds) in
    let%ok ledger, handle =
      Ledger.withdraw
        ~sender:(Address.to_key_hash sender |> Option.value_exn)
        ~destination:owner amount ticket ledger in
    Ok ({ ledger; contract_storage }, Some (Receipt_tezos_withdraw handle))
  | Tezos_withdraw _ ->
    (*TODO *)
    Error `Insufficient_funds
  | Contract_origination { payload; tickets } ->
    (* @TODO: deduct gas from account and check *)
    let balance = Int.max_value |> Amount.of_int in
    (* TODO: those constants should not be defined here *)
    let origination_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Origination_error ("Not enought funds", sender),
          let comparison_result = compare balance origination_cost in
          comparison_result >= 0 ) in

    let initial_gas = Amount.to_int Amount.(balance - origination_cost) in
    let wrap_error t =
      t |> Result.map_error ~f:(fun x -> `Origination_error (x, sender)) in
    let%ok contract_storage, ledger, contract_address =
      Ledger.with_ticket_table ledger (fun ~get_table ~set_table ->
          let table = get_table () in
          let%ok tickets', table =
            Ticket_table.take_tickets table ~sender
              ~tickets:(List.map ~f:fst tickets) in

          let contract_address =
            Contract_address.of_user_operation_hash operation_hash in
          let address = Address.of_contract_hash contract_address in

          let table =
            Ticket_table.update_tickets table ~sender:address ~tickets:tickets'
          in
          (* TODO: Burn on storage size change, need CTEZ *)
          let%ok contract =
            Contract_vm.Compiler.compile
              ~tickets:
                (List.map ~f:(fun (f, r) -> (f, fst r)) tickets
                |> Stdlib.List.to_seq)
              ~gas:initial_gas payload
              ~sender:(Address.of_key_hash source)
            |> wrap_error in
          let contract_storage =
            Contract_storage.originate_contract t.contract_storage
              ~address:contract_address ~contract in
          let ledger = set_table table in
          Ok (contract_storage, ledger, contract_address))
      |> Result.map_error ~f:(function
           | `Insufficient_funds ->
             `Origination_error ("not_enough_funds", sender)
           | x -> x) in
    Ok
      ( { contract_storage; ledger },
        Some
          (Receipt_contract_origination
             {
               sender;
               outcome =
                 `Success
                   {
                     originated_contract_address = contract_address;
                     initial_tickets = List.map ~f:(fun (x, _) -> x) tickets;
                   };
             }) )
  | Contract_invocation { to_invoke; argument; tickets } ->
    let balance = Int.max_value |> Amount.of_int in
    (* TODO: find good transaction cost *)
    let invocation_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Invocation_error ("Not enought funds", sender),
          let comparison_result = compare balance invocation_cost in
          comparison_result >= 0 ) in
    let burn_cap = invocation_cost in
    let initial_gas = Amount.(to_int (balance - burn_cap)) in
    let wrap_error t =
      Result.map_error ~f:(fun x -> `Invocation_error (x, sender)) t in
    let%ok contract =
      Contract_storage.get_contract t.contract_storage ~address:to_invoke
      |> Result.of_option ~error:"Contract not found"
      |> wrap_error in
    Ledger.with_ticket_table t.ledger (fun ~get_table ~set_table ->
        let table = get_table () in
        let table_tickets, table =
          Ticket_table.take_all_tickets table
            ~sender:(Address.of_contract_hash to_invoke) in
        let%ok _, table =
          Ticket_table.take_tickets table ~sender
            ~tickets:(List.map ~f:fst tickets) in
        let ctx, to_replace =
          let source = Address.of_key_hash source in
          Context.make_state ~sender ~source
            ~mapping:(Contract_vm.Contract.tickets_mapping contract)
            ~contract_owned_tickets:table_tickets
            ~self:(Address.of_contract_hash to_invoke)
            ~provided_tickets:(Stdlib.List.to_seq tickets)
            ~get_contract_opt:(fun address ->
              let address =
                Address.to_contract_hash address |> Option.value_exn in
              Contract_storage.get_contract contract_storage ~address
              |> Option.map ~f:(Fn.const (Address.of_contract_hash address)))
        in
        let%ok contract, user_op_list =
          Contract_vm.Interpreter.invoke ~to_replace ~ctx ~arg:argument
            ~gas:initial_gas contract
          |> wrap_error in

        let%ok tickets_mapping, tickets, operations =
          ctx#finalize user_op_list
          |> Result.map_error ~f:(fun `Execution_error ->
                 `Invocation_error ("execution error", sender)) in
        let contract =
          Contract_vm.Contract.update_tickets contract tickets_mapping in
        let contract_storage =
          Contract_storage.update_contract_storage contract_storage
            ~address:to_invoke ~updated_contract:contract in
        let table =
          Ticket_table.update_tickets table
            ~sender:(Address.of_contract_hash to_invoke)
            ~tickets in
        let ledger = set_table table in
        let%ok new_state, receipts =
          List.fold_result
            ~f:(fun acc op ->
              apply_contract_ops ~source
                ~sender:(Address.of_contract_hash to_invoke)
                acc op)
            ~init:({ ledger; contract_storage }, [])
            operations in
        Ok
          ( new_state,
            Some
              (Receipt_contract_invocation
                 {
                   sender;
                   outcome =
                     `Success
                       {
                         additional_operations = receipts;
                         remaining_tickets =
                           Contract_vm.Contract.tickets_mapping contract
                           |> List.map ~f:fst;
                         new_storage = Contract_vm.Contract.raw_storage contract;
                       };
                 }) ))

and apply_contract_ops ~source ~sender (t, acc) x =
  match x with
  | Context.Operation.Invoke { tickets; destination; param } ->
    let%ok destination =
      Address.to_contract_hash destination
      |> Result.of_option
           ~error:
             (`Invalid_address_error
               (Format.sprintf "invalid address.\nsender: %s\n destination: %s"
                  (Address.to_string sender)
                  (Address.to_string destination))) in
    let%ok payload =
      Contract_vm.Invocation_payload.of_bytes ~arg:param
      |> Result.map_error ~f:(fun x ->
             `Invalid_payload (Format.sprintf "invalid_payload %s" x)) in
    let operation =
      User_operation.Contract_invocation
        { to_invoke = destination; argument = payload; tickets } in
    let operation = User_operation.make ~source operation in
    let%ok new_state, receipt =
      apply_user_operation ~sender t operation.hash operation in
    Ok
      ( new_state,
        { user_operation = operation; operation_receipt = receipt } :: acc )
  | Transfer { ticket; amount; destination } ->
    let%ok destination =
      Address.to_key_hash destination
      |> Result.of_option
           ~error:
             (`Invalid_address_error
               (Format.sprintf
                  "invalid implicit address.\nsender: %s\n destination: %s"
                  (Address.to_string sender)
                  (Address.to_string destination))) in
    let operation = User_operation.Transaction { destination; amount; ticket } in
    let operation = User_operation.make ~source operation in
    let%ok new_state, receipt =
      apply_user_operation ~sender t operation.hash operation in
    Ok
      ( new_state,
        { user_operation = operation; operation_receipt = receipt } :: acc )

let apply_user_operation t hash user_operation =
  let report_error error =
    let message =
      match error with
      | `Origination_error (msg, _) -> "Origination error: " ^ msg
      | `Invocation_error (msg, _) -> "Invocation error: " ^ msg
      | `Insufficient_funds -> "Insufficient funds"
      | `Invalid_payload msg -> "Invalid contract payload" ^ msg
      | `Invalid_address_error msg -> "invalid address in contract call " ^ msg
    in
    Log.error "Operation %a - %s" BLAKE2B.pp hash message in
  match apply_user_operation t hash user_operation with
  | Ok (new_state, receipt) -> (new_state, receipt)
  | Error (`Origination_error (_, sender) as error) ->
    report_error error;
    (t, Some (Receipt_contract_origination { sender; outcome = `Failure }))
  | Error (`Invocation_error (_, sender) as error) ->
    report_error error;
    (t, Some (Receipt_contract_invocation { sender; outcome = `Failure }))
  | Error error ->
    report_error error;
    (t, None)
