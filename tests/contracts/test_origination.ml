open Core_deku
open Deku_data
open Contracts
open Helpers_contracts

let test_wasm msg =
  let initial_state, address, ticket = setup () in
  let payload =
    let code =
      {|
      (module
      (import "env" "syscall" (func $syscall (param i64) (result i32)))
      (memory (export "memory") 1)
      (func (export "main")  (param i32) (result i64 i64 i64)
        i32.const 21
        i32.const 5
        i32.store
        i32.const 22
        i32.const 0
        i32.store
        i64.const 21 
        call $syscall
        i64.extend_i32_s
        (i64.const 20)
        (i64.const 45)
        ))
      |}
      |> Bytes.of_string in
    let storage = Int32.zero |> Context.Ticket_handle.to_bytes in

    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let operation =
    User_operation.Contract_origination
      {
        payload;
        tickets =
          [((ticket, Amount.of_int 10000), (Int32.zero, Some Int64.zero))];
      } in
  let user_op = User_operation.make ~source:address operation in

  let state, _ = State.apply_user_operation initial_state user_op.hash user_op in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:false
          ~actual:
            (Contract_storage.equal
               (State.contract_storage state)
               Contract_storage.empty));
  ]

let test_wasm_fail msg =
  let _, _, t1 = setup () in
  let initial_state, address, _ticket = setup () in
  let payload =
    let code =
      {|
        (module
        (import "env" "syscall" (func $syscall (param i64) (result i32)))
        (memory (export "memory") 1)
        (func (export "main")  (param i32) (result i64 i64 i64)
          i32.const 21
          i32.const 5
          i32.store
          i32.const 22
          i32.const 0
          i32.store
          i64.const 21 
          call $syscall
          i64.extend_i32_s
          (i64.const 20)
          (i64.const 45)
          ))
        |}
      |> Bytes.of_string in
    let storage = Int32.one |> Context.Ticket_handle.to_bytes in

    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let operation =
    User_operation.Contract_origination
      {
        payload;
        tickets = [((t1, Amount.of_int 10000), (Int32.one, Some Int64.zero))];
      } in
  let user_op = User_operation.make ~source:address operation in

  let state, _ = State.apply_user_operation initial_state user_op.hash user_op in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:true
          ~actual:
            (Contract_storage.equal
               (State.contract_storage state)
               Contract_storage.empty));
  ]

let test_origination =
  ( "Origination",
    [
      test_wasm "Wasm origination should succeed";
      test_wasm_fail "Wasm origination should fail";
    ]
    |> List.concat )
