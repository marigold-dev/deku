open Crypto
open Helpers_contracts
open Deku_vm
open External_vm.External_vm_protocol

let test msg =
  let script = [%lambda_vm.script fun x -> x + 1L] in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let code = Lambda_vm.Ast.script_to_yojson script in
  let storage = Lambda_vm.Ast.value_to_yojson value in
  let payload =
    Contract_vm.Origination_payload.lambda_of_yojson ~code ~storage
    |> Result.get_ok in
  let operation = Contract_transaction.Contract_origination payload in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let vm_state, storage, address = setup () in
  let _ = Contract_transaction.transaction storage address mock_hash operation in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:false
          ~actual:(State.equal !vm_state State.empty));
  ]

let test_dummy msg =
  let storage = 0 in
  let payload = Contract_vm.Origination_payload.dummy_of_yojson ~storage in
  let operation = Contract_transaction.Contract_origination payload in
  let mock_hash = BLAKE2B.hash "mocked op hash" in
  let vm_state, storage, address = setup () in
  let _ = Contract_transaction.transaction storage address mock_hash operation in

  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:false
          ~actual:(State.equal !vm_state State.empty));
  ]

let test_origination =
  ( "Origination",
    [
      test "Origination should succeed";
      test_dummy "Dummy origination should succeed";
    ]
    |> List.concat )
