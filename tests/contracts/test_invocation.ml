open Core_deku
open Contracts
open Helpers_contracts

let test_successfuly_own_ticket () =
  let payload =
    let code =
      Bytes.of_string
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
            (i64.const 4)
            (i64.const 99)
            ))
      |}
    in
    let storage = Bytes.empty in
    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let initial_state, address, ticket = setup () in

  let state, contract_address, old_contract =
    let user_op =
      let operation =
        User_operation.Contract_origination { payload; tickets = [] } in
      User_operation.make ~source:address operation in

    let contract_address =
      Contract_address.of_user_operation_hash user_op.hash in

    let state, _ =
      State.apply_user_operation initial_state user_op.hash user_op in
    let old_contract =
      state
      |> State.contract_storage
      |> Contract_storage.get_contract ~address:contract_address
      |> Option.get in

    (state, contract_address, old_contract) in

  let new_contract =
    let argument =
      Int32.zero
      |> Context.Ticket_handle.to_bytes
      |> [%to_yojson: bytes]
      |> (fun arg -> Contract_vm.Invocation_payload.wasm_of_yojson ~arg)
      |> Result.get_ok in

    let operation =
      let operation =
        User_operation.Contract_invocation
          {
            to_invoke = contract_address;
            argument;
            tickets =
              [((ticket, Amount.of_int 55), (Int32.zero, Some Int64.zero))];
          } in
      User_operation.make ~source:address operation in

    let state, _ = State.apply_user_operation state operation.hash operation in
    state
    |> State.contract_storage
    |> Contract_storage.get_contract ~address:contract_address
    |> Option.get in

  Alcotest.(check' (neg contract_value))
    ~msg:"New contract's storage should differ" ~expected:old_contract
    ~actual:new_contract;

  let held_ticket, _ =
    let handle =
      new_contract
      |> Contracts.Contract_vm.Contract.raw_storage
      |> Context.Ticket_handle.of_bytes in
    new_contract
    |> Contracts.Contract_vm.Contract.tickets_mapping
    |> List.find (fun (_, h) -> h = handle) in
  Alcotest.(check' (pair Helpers_contracts.ticket amount))
    ~msg:
      "Ticket handle held in storage should be the same as passed by argument"
    ~actual:held_ticket
    ~expected:(ticket, Amount.of_int 55)

let test_succesfuly_mint_ticket () =
  let payload =
    let code =
      Bytes.of_string
        {|
        (module
          (import "env" "syscall" (func $syscall (param i64) (result i32)))
          (memory (export "memory") 1)
          (data (i32.const 70) "\37\13")
          (data (i32.const 79) "\08")
          (data (i32.const 84) "deadbeef")

          (func $mint (param $addr i32)
            i32.const 40
            i32.const 9
            i32.store
            i32.const 45
            local.get $addr
            i32.store

            i64.const 40
            call $syscall
            drop)

          (func $own (param $addr i32) (result i32)
            i32.const 40
            i32.const 5
            i32.store
            i32.const 45
            local.get $addr
            i32.store
            i64.const 40
            call $syscall)

          (func (export "main") (param i32) (result i64 i64 i64)
            i32.const 70
            call $mint

            i32.const 70
            call $own

            i64.extend_i32_s
            i64.const 4
            i64.const 120
          )
        )
      |}
    in
    let storage = Bytes.empty in
    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let initial_state, address, ticket = setup () in

  let state, contract_address =
    let user_op =
      let operation =
        User_operation.Contract_origination { payload; tickets = [] } in
      User_operation.make ~source:address operation in

    let contract_address =
      Contract_address.of_user_operation_hash user_op.hash in

    let state, _ =
      State.apply_user_operation initial_state user_op.hash user_op in

    (state, contract_address) in

  let new_contract =
    let argument =
      Bytes.empty
      |> [%to_yojson: bytes]
      |> (fun arg -> Contract_vm.Invocation_payload.wasm_of_yojson ~arg)
      |> Result.get_ok in

    let operation =
      let operation =
        User_operation.Contract_invocation
          {
            to_invoke = contract_address;
            argument;
            tickets =
              [((ticket, Amount.of_int 55), (Int32.zero, Some Int64.zero))];
          } in
      User_operation.make ~source:address operation in

    let state, _ = State.apply_user_operation state operation.hash operation in
    state
    |> State.contract_storage
    |> Contract_storage.get_contract ~address:contract_address
    |> Option.get in

  let held_ticket, _ =
    let handle =
      new_contract
      |> Contracts.Contract_vm.Contract.raw_storage
      |> Context.Ticket_handle.of_bytes in
    new_contract
    |> Contracts.Contract_vm.Contract.tickets_mapping
    |> List.find (fun (_, h) -> h = handle) in

  let expected_ticket =
    Ticket_id.mint_ticket
      ~contract_address:(Address.of_contract_hash contract_address)
      ~data:(Bytes.of_string "deadbeef") in

  Alcotest.(check' (pair Helpers_contracts.ticket amount))
    ~msg:
      "Ticket handle held in storage should be the same as minted by the \
       contract"
    ~actual:held_ticket
    ~expected:(expected_ticket, Amount.of_int 0x1337)

let test_invocation =
  Alcotest.
    ( "Invocation",
      [
        test_case "Wasm contract succesfuly own ticket" `Quick
          test_successfuly_own_ticket;
        test_case "Wasm contract succesfuly mint a ticket" `Quick
          test_succesfuly_mint_ticket;
      ] )
