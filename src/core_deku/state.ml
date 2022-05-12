open Helpers
open Crypto
open Core

type t = {
  ledger : Ledger.t;
  contract_storage : Contract_storage.t;
}
[@@deriving yojson]

type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
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
      let ledger =
        match destination with
        | Implicit key_hash ->
          let destination = key_hash in
          Ledger.deposit (Address.of_key_hash destination) amount ticket ledger
        | Originated _ -> failwith "not implemented" in
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
  | Tezos_withdraw { owner; amount; ticket } ->
    let%ok ledger, handle =
      Ledger.withdraw
        ~sender:(Address.to_key_hash sender |> Option.value_exn)
        ~destination:owner amount ticket ledger in
    Ok ({ ledger; contract_storage }, Some (Receipt_tezos_withdraw handle))
  | Contract_origination { payload; tickets } ->
    (* @TODO: deduct gas from account and check *)
    let balance = Int.max_value |> Amount.of_int in
    (* TODO: those constants should not be defined here *)
    let origination_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Origination_error "Not enought funds",
          let comparison_result = compare balance origination_cost in
          comparison_result >= 0 ) in

    let initial_gas = Amount.to_int Amount.(balance - origination_cost) in
    let wrap_error t =
      t |> Result.map_error ~f:(fun x -> `Origination_error x) in
    let table = Ledger.ticket_table ledger in
    let%ok tickets, table = Ticket_table.take_tickets table ~sender ~tickets in

    let contract_address =
      Contract_address.of_user_operation_hash operation_hash in
    let address = Address.of_contract_hash contract_address in
    let og_handles, new_handles =
      Seq.fold_left
        (fun (acc1, acc2) (ticket, amount) ->
          ( Ticket_handle.make sender ticket amount :: acc1,
            Ticket_handle.make address ticket amount :: acc2 ))
        ([], []) tickets in
    let table = Ticket_table.update_tickets table ~sender:address ~tickets in
    let handles =
      List.fold2_exn
        ~f:(fun acc x y ->
          (Ticket_handle.to_string x, Ticket_handle.to_string y) :: acc)
        ~init:[] og_handles new_handles in
    let ledger = Ledger.update_ticket_table table ledger in
    (* TODO: Burn on storage size change, need CTEZ *)
    let%ok contract =
      Contract_vm.Compiler.compile ~gas:initial_gas payload ~tickets:handles
      |> wrap_error in
    let contract_storage =
      Contract_storage.originate_contract t.contract_storage
        ~address:contract_address ~contract in
    Ok ({ contract_storage; ledger }, None)
  | Contract_invocation { to_invoke; argument; tickets } -> (
    let balance = Int.max_value |> Amount.of_int in
    (* TODO: find good transaction cost *)
    let invocation_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Invocation_error "Not enought funds",
          let comparison_result = compare balance invocation_cost in
          comparison_result >= 0 ) in
    let burn_cap = invocation_cost in
    let initial_gas = Amount.(to_int (balance - burn_cap)) in
    let wrap_error t = Result.map_error ~f:(fun x -> `Invocation_error x) t in
    let%ok contract =
      Contract_storage.get_contract t.contract_storage ~address:to_invoke
      |> Result.of_option ~error:"Contract not found"
      |> wrap_error in
    let table = Ledger.ticket_table t.ledger in
    let table_tickets, table =
      Ticket_table.tickets table ~sender:(Address.of_contract_hash to_invoke)
    in
    let%ok tickets, table = Ticket_table.take_tickets table ~sender ~tickets in
    let ctx =
      let source = Address.of_key_hash source in
      Contract_context.make_state ~sender ~source
        ~contract_owned_tickets:table_tickets
        ~self:(Address.of_contract_hash to_invoke) ~provided_tickets:tickets
        ~get_contract_opt:(fun address ->
          let address = Address.to_contract_hash address |> Option.value_exn in
          Contract_storage.get_contract contract_storage ~address
          |> Option.map ~f:(Fn.const (Address.of_contract_hash address))) in
    let%ok contract, user_op_list =
      Contract_vm.Interpreter.invoke ~ctx ~arg:argument ~gas:initial_gas
        contract
      |> wrap_error in
    let contract_storage =
      Contract_storage.update_contract_storage contract_storage
        ~address:to_invoke ~updated_contract:contract in
    let to_run = ctx#finalize user_op_list in
    match to_run with
    | Ok (tickets, operations) ->
      let table =
        Ticket_table.update_tickets table
          ~sender:(Address.of_contract_hash to_invoke)
          ~tickets in
      let new_ledger = Ledger.update_ticket_table table t.ledger in
      List.fold_result
        ~f:(fun acc op ->
          apply_contract_ops ~source
            ~sender:(Address.of_contract_hash to_invoke)
            acc op)
        ~init:({ ledger = new_ledger; contract_storage }, None)
        operations
    | Error _ -> Error (`Invocation_error "error occured"))

and apply_contract_ops ~source ~sender (t, _) x =
  match x with
  | Contract_operation.Invoke { tickets; destination; param } ->
    let%ok destination =
      Address.to_contract_hash destination
      |> Result.of_option ~error:(`Invocation_error "invalid address") in
    let%ok payload =
      Contract_vm.Invocation_payload.of_bytes ~arg:param
      |> Result.map_error ~f:(fun x -> `Invocation_error x) in
    let operation =
      User_operation.Contract_invocation
        { to_invoke = destination; argument = payload; tickets } in
    let operation = User_operation.make ~source operation in
    apply_user_operation ~sender t operation.hash operation
  | Transfer { ticket; amount; destination } ->
    let%ok destination =
      Address.to_key_hash destination
      |> Result.of_option ~error:(`Invocation_error "invalid address") in
    let operation = User_operation.Transaction { destination; amount; ticket } in
    let operation = User_operation.make ~source operation in
    apply_user_operation ~sender t operation.hash operation

let apply_user_operation t hash user_operation =
  match apply_user_operation t hash user_operation with
  | Ok (t, receipt) -> (t, receipt)
  (* TODO: use this erros for something *)
  | Error (`Origination_error _ | `Invocation_error _) -> (t, None)
  | Error `Insufficient_funds -> (t, None)
