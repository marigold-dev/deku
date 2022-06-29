open Crypto
open Core_deku
open Contracts
open Helpers_contracts

let test_ok msg =
  let initial_state, address, _ = setup () in
  let script =
    [%lambda_vm.script
      fun param ->
        ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]
  in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let code = Lambda_vm.Ast.script_to_yojson script in
  let storage = Lambda_vm.Ast.value_to_yojson value in
  let payload =
    Contract_vm.Origination_payload.lambda_of_yojson ~code ~storage
    |> Result.get_ok in
  let operation =
    User_operation.Contract_origination { payload; tickets = [] } in
  let user_op = User_operation.make ~source:address operation in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let contract_address = mock_hash |> Contract_address.of_user_operation_hash in
  let state, _ = State.apply_user_operation initial_state mock_hash user_op in
  let init_storage = State.contract_storage state in
  let payload =
    Contract_vm.Invocation_payload.lambda_of_yojson
      ~arg:Lambda_vm.Ast.(Int64 1L |> value_to_yojson)
    |> Result.get_ok in
  let operation =
    User_operation.Contract_invocation
      { to_invoke = contract_address; argument = payload; tickets = [] } in
  let operation = User_operation.make ~source:address operation in
  let state, _ = State.apply_user_operation state mock_hash operation in
  let new_storage = State.contract_storage state in
  let old_contract =
    Contract_storage.get_contract ~address:contract_address init_storage
    |> Option.get in
  let new_contract =
    Contract_storage.get_contract ~address:contract_address new_storage
    |> Option.get in
  [
    Alcotest.test_case "contract storage changes" `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg
          ~expected:(Contract_vm.Contract.equal new_contract old_contract)
          ~actual:false);
  ]

let test_failure msg =
  let initial_state, address, _ = setup () in
  let script = [%lambda_vm.script fun x -> (x + 1L, (0L, 0L))] in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let code = Lambda_vm.Ast.script_to_yojson script in
  let storage = Lambda_vm.Ast.value_to_yojson value in
  let payload =
    Contract_vm.Origination_payload.lambda_of_yojson ~code ~storage
    |> Result.get_ok in
  let operation =
    User_operation.Contract_origination { payload; tickets = [] } in
  let user_op = User_operation.make ~source:address operation in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let contract_address = mock_hash |> Contract_address.of_user_operation_hash in
  let state, _ = State.apply_user_operation initial_state mock_hash user_op in
  let init_storage = State.contract_storage state in
  let payload =
    Contract_vm.Invocation_payload.lambda_of_yojson
      ~arg:Lambda_vm.Ast.(Pair (Int64 1L, Int64 1L) |> value_to_yojson)
    |> Result.get_ok in
  let operation =
    User_operation.Contract_invocation
      { to_invoke = contract_address; argument = payload; tickets = [] } in
  let operation = User_operation.make ~source:address operation in
  let state, _ = State.apply_user_operation state mock_hash operation in
  let new_storage = State.contract_storage state in
  let old_contract =
    Contract_storage.get_contract ~address:contract_address init_storage
    |> Option.get in
  let new_contract =
    Contract_storage.get_contract ~address:contract_address new_storage
    |> Option.get in
  [
    Alcotest.test_case "contract storage doesnt change when invocation failed"
      `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg
          ~expected:(Contract_vm.Contract.equal new_contract old_contract)
          ~actual:true);
  ]

let test_dummy_ok msg =
  let initial_state, address, _ = setup () in
  let payload = Contract_vm.Origination_payload.dummy_of_yojson ~storage:0 in
  let operation =
    User_operation.Contract_origination { payload; tickets = [] } in
  let user_op = User_operation.make ~source:address operation in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let contract_address = mock_hash |> Contract_address.of_user_operation_hash in
  let state, _ = State.apply_user_operation initial_state mock_hash user_op in
  let init_storage = State.contract_storage state in
  let payload =
    Contract_vm.Invocation_payload.dummy_of_yojson ~arg:(`List [`Int 0; `Int 1])
    |> Result.get_ok in
  let operation =
    User_operation.Contract_invocation
      { to_invoke = contract_address; argument = payload; tickets = [] } in
  let operation = User_operation.make ~source:address operation in
  let state, _ = State.apply_user_operation state mock_hash operation in
  let new_storage = State.contract_storage state in
  let old_contract =
    Contract_storage.get_contract ~address:contract_address init_storage
    |> Option.get in
  let new_contract =
    Contract_storage.get_contract ~address:contract_address new_storage
    |> Option.get in
  [
    Alcotest.test_case "contract storage changes when dummy contract is invoked"
      `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg
          ~expected:(Contract_vm.Contract.equal new_contract old_contract)
          ~actual:false);
  ]

let test_dummy_failure msg =
  let initial_state, address, _ = setup () in
  let payload = Contract_vm.Origination_payload.dummy_of_yojson ~storage:0 in
  let operation =
    User_operation.Contract_origination { payload; tickets = [] } in
  let user_op = User_operation.make ~source:address operation in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let contract_address = mock_hash |> Contract_address.of_user_operation_hash in
  let state, _ = State.apply_user_operation initial_state mock_hash user_op in
  let init_storage = State.contract_storage state in
  let payload =
    Contract_vm.Invocation_payload.lambda_of_yojson
      ~arg:Lambda_vm.Ast.(Pair (Int64 1L, Int64 1L) |> value_to_yojson)
    |> Result.get_ok in
  let operation =
    User_operation.Contract_invocation
      { to_invoke = contract_address; argument = payload; tickets = [] } in
  let operation = User_operation.make ~source:address operation in
  let state, _ = State.apply_user_operation state mock_hash operation in
  let new_storage = State.contract_storage state in
  let old_contract =
    Contract_storage.get_contract ~address:contract_address init_storage
    |> Option.get in
  let new_contract =
    Contract_storage.get_contract ~address:contract_address new_storage
    |> Option.get in
  [
    Alcotest.test_case "contract invocation for dummy should fail" `Quick
      (fun () ->
        Alcotest.(check' bool)
          ~msg
          ~expected:(Contract_vm.Contract.equal new_contract old_contract)
          ~actual:true);
  ]

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
  ( "Invocation",
    [
      test_ok "Invocation should succeed and contract storage should change";
      test_failure
        "Invocation should fail and contract storage should remain the same";
      test_dummy_ok "Invocation for dummy vm should succeed";
      test_dummy_failure "Invocation for dummy should fail";
      test_ok_wasm "Wasm invocation should succeed";
    ]
    |> List.flatten )
