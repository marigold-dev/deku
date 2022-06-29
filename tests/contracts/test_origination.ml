open Crypto
open Core_deku
open Contracts
open Helpers_contracts

let test msg =
  let initial_state, address = setup () in
  let script = [%lambda_vm.script fun x -> x + 1L] in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let code = Lambda_vm.Ast.script_to_yojson script in
  let storage = Lambda_vm.Ast.value_to_yojson value in
  let payload =
    Contract_vm.Origination_payload.lambda_of_yojson ~code ~storage
    |> Result.get_ok in
  let operation = User_operation.Contract_origination payload in
  let user_op = User_operation.make ~source:address operation in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let state, _ = State.apply_user_operation initial_state mock_hash user_op in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:false
          ~actual:
            (Contract_storage.equal
               (State.contract_storage state)
               Contract_storage.empty));
  ]

let test_dummy msg =
  let initial_state, address = setup () in
  let storage = 0 in
  let payload = Contract_vm.Origination_payload.dummy_of_yojson ~storage in
  let operation = User_operation.Contract_origination payload in
  let user_op = User_operation.make ~source:address operation in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let state, _ = State.apply_user_operation initial_state mock_hash user_op in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:false
          ~actual:
            (Contract_storage.equal
               (State.contract_storage state)
               Contract_storage.empty));
  ]

let test_origination =
  ( "Origination",
    [
      test "Origination should succeed";
      test_dummy "Dummy origination should succeed";
    ]
    |> List.concat )
