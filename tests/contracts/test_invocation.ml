open Core_deku
open Deku_data
open Contracts
open Helpers_contracts

let test_ok_wasm msg =
  let initial_state, address, ticket = setup () in
  let code =
    {|
    (module
    (import "env" "syscall" (func $syscall (param i64) (result i32)))
    (memory (export "memory") 1)
    (func (export "main")  (param i32) (result i64 i64 i64)
      i32.const 41
      i32.const 5
      i32.store
      i32.const 46
      i32.const 0
      i32.store
      i64.const 41 
      call $syscall
      i64.extend_i32_s
      (i64.const 40)
      (i64.const 99)
      ))
    |}
    |> Bytes.of_string in
  let storage = Bytes.empty in
  let payload =
    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in
  let operation =
    User_operation.Contract_origination { payload; tickets = [] } in
  let user_op = User_operation.make ~source:address operation in
  let contract_address =
    user_op.hash |> Contract_address.of_user_operation_hash in
  let state, _ = State.apply_user_operation initial_state user_op.hash user_op in
  let init_storage = State.contract_storage state in
  let arg = [%to_yojson: bytes] (Int32.zero |> Context.Ticket_handle.to_bytes) in
  let payload =
    Contract_vm.Invocation_payload.wasm_of_yojson ~arg |> Result.get_ok in
  let operation =
    User_operation.Contract_invocation
      {
        to_invoke = contract_address;
        argument = payload;
        tickets = [((ticket, Amount.of_int 55), (Int32.zero, Some Int64.zero))];
      } in
  let operation = User_operation.make ~source:address operation in
  let state, _ = State.apply_user_operation state user_op.hash operation in
  let new_storage = State.contract_storage state in
  let old_contract =
    Contract_storage.get_contract ~address:contract_address init_storage
    |> Option.get in
  let new_contract =
    Contract_storage.get_contract ~address:contract_address new_storage
    |> Option.get in
  [
    Alcotest.test_case "wasm contract storage changes" `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg
          ~expected:(Contract_vm.Contract.equal new_contract old_contract)
          ~actual:false);
  ]

let test_invocation =
  ("Invocation", [test_ok_wasm "Wasm invocation should succeed"] |> List.flatten)
