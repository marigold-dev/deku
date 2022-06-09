open Crypto
open Helpers
open Core_deku

let make_ticket ?ticketer ?data () =
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Random.generate 20
        |> Cstruct.to_string
        |> BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  { ticketer; data }

let make_address () =
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  key_hash

let make_tezos_address () =
  let open Crypto in
  let open Tezos in
  let _key, address = Ed25519.generate () in
  let hash = Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

let setup ?(initial_amount = 10000) () =
  let t2 = make_ticket () in
  let tezos_address = make_tezos_address () in
  let op =
    Tezos_operation.Tezos_deposit
      {
        destination = tezos_address;
        ticket = t2;
        amount = Amount.of_int initial_amount;
      } in
  let s = State.empty in
  let opp =
    {
      Tezos_operation.tezos_operation_hash =
        "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"
        |> Tezos.Operation_hash.of_string
        |> Option.get;
      internal_operations = [op];
    } in
  let opp = Tezos_operation.make opp in
  let make_address =
    tezos_address
    |> Tezos.Address.to_string
    |> Address.of_string
    |> Option.map Address.to_key_hash
    |> Option.join
    |> Option.get in
  (State.apply_tezos_operation s opp, make_address, t2)

let amount =
  Alcotest.of_pp (fun ppf x -> Format.fprintf ppf "%d" (Amount.to_int x))

let test msg =
  let initial_state, address, _ = setup () in
  let script = [%lambda_vm.script fun x -> x + 1L] in
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
  let initial_state, address, _ = setup () in
  let storage = 0 in
  let payload = Contract_vm.Origination_payload.dummy_of_yojson ~storage in
  let operation =
    User_operation.Contract_origination { payload; tickets = [] } in
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
    let storage =
      Ticket_handle.make
        (Address.of_key_hash address)
        ticket (Amount.of_int 10000)
      |> Ticket_handle.to_bytes in

    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let operation =
    User_operation.Contract_origination
      { payload; tickets = [(ticket, Amount.of_int 10000)] } in
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
    let storage =
      Ticket_handle.make
        (Address.of_key_hash address)
        ticket (Amount.of_int 10000)
      |> Ticket_handle.to_bytes in

    Contract_vm.Origination_payload.wasm_of_yojson ~code ~storage
    |> Result.get_ok in

  let operation =
    User_operation.Contract_origination
      { payload; tickets = [(t1, Amount.of_int 10000)] } in
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
      test "Origination should succeed";
      test_dummy "Dummy origination should succeed";
      test_wasm "Wasm origination should succeed";
      test_wasm_fail "Wasm origination should fail";
    ]
    |> List.concat )
