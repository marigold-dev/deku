open Core_deku
open Contracts
open Helpers_contracts

let test_wasm () =
  let payload =
    let code =
      Bytes.of_string
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
    in
    let storage = Context.Ticket_handle.to_bytes Int32.zero in

    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let initial_state, address, ticket = setup () in
  let tickets =
    [((ticket, Amount.of_int 10000), (Int32.zero, Some Int64.zero))] in

  let actual =
    let operation = User_operation.Contract_origination { payload; tickets } in
    let user_op = User_operation.make ~source:address operation in

    let state, _ =
      State.apply_user_operation initial_state user_op.hash user_op in
    let address = Contract_address.of_user_operation_hash user_op.hash in
    Contract_storage.get_contract (State.contract_storage state) ~address
    |> Option.get in

  let expected = compile payload tickets |> Result.get_ok in
  Alcotest.check' contract_value
    ~msg:"Contract should have the same value as its counterpart in storage"
    ~actual ~expected

let test_wasm_fail_on_wrong_ticket () =
  let payload =
    let code =
      Bytes.of_string
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
    in
    let storage = Context.Ticket_handle.to_bytes Int32.one in

    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let _, _, not_owned_ticket = setup () in
  let initial_state, address, _ticket = setup () in
  let tickets =
    [((not_owned_ticket, Amount.of_int 10000), (Int32.one, Some Int64.zero))]
  in

  let actual =
    let operation = User_operation.Contract_origination { payload; tickets } in
    let user_op = User_operation.make ~source:address operation in

    let state, _ =
      State.apply_user_operation initial_state user_op.hash user_op in
    let address = Contract_address.of_user_operation_hash user_op.hash in
    Contract_storage.get_contract (State.contract_storage state) ~address in

  Alcotest.(check' (option contract_value))
    ~msg:"Failed origination should add no contract to storage" ~actual
    ~expected:None

let test_origination =
  Alcotest.
    ( "Origination",
      [
        test_case "Wasm origination should succeed" `Quick test_wasm;
        test_case "Wasm origination should fail due to wrong ticket handle"
          `Quick test_wasm_fail_on_wrong_ticket;
      ] )
