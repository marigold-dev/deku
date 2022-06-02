(*********************************************************************************)
(* Initial state with deposits *)
open Build_usage

let init_tezos_operation_hash =
  "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

let init_state () =
  let tezos_add_1 = make_tezos_address () in
  let tezos_add_2 = make_tezos_address () in
  let ticket_1 = make_ticket () in
  let ticket_2 = make_ticket () in
  (* build a list of deposits *)
  let op1 =
    Core_deku.Tezos_operation.Tezos_deposit
      {
        destination = tezos_add_1;
        ticket = ticket_1;
        amount = Core_deku.Amount.of_int 10_000;
      } in
  let op2 =
    Core_deku.Tezos_operation.Tezos_deposit
      {
        destination = tezos_add_2;
        ticket = ticket_2;
        amount = Core_deku.Amount.of_int 20_000;
      } in
  (* init state *)
  let state = Core_deku.State.empty in
  let tezos_operation_hash =
    init_tezos_operation_hash |> Tezos.Operation_hash.of_string |> Option.get
  in
  let payload =
    {
      Core_deku.Tezos_operation.tezos_operation_hash;
      internal_operations = [op1; op2];
    } in
  let tezos_operation = Core_deku.Tezos_operation.make payload in
  let state = Core_deku.State.apply_tezos_operation state tezos_operation in
  (state, (tezos_add_1, tezos_add_2), (ticket_1, ticket_2))

(*******************************************************************************)
(* Build basic case for state *)

let build_state () =
  let init_state, (tezos_add_1, _tezos_add_2), (ticket_1, _) = init_state () in
  let code, storage = Build_operations.contract_vm in
  let initial_operation =
    Build_operations.user_op_contract_origination code storage in
  (* first user operation as contract origination,
     source is the destination address of tezos_operation
  *)
  let deku_add_1 = make_address () in
  let deku_add_2 = make_address () in
  let op1 = Core_deku.User_operation.make ~source:deku_add_1 initial_operation in
  let mock_hash = Crypto.BLAKE2B.hash "mocked op hash" in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation init_state mock_hash op1 in
  let init_storage = Core_deku.State.contract_storage state in
  (* second user operation as contract invocation payload same source *)
  let arg = Build_operations.contract_arg () in
  let user_op, contract_address =
    Build_operations.user_op_contract_invocation mock_hash arg in
  let op2 = Core_deku.User_operation.make ~source:deku_add_1 user_op in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation state mock_hash op2 in
  (* third user operation as transfer same source *)
  let op3 =
    Core_deku.User_operation.make ~source:deku_add_1
      (Build_operations.user_op_transaction ~destination:deku_add_2
         ~amount:(Core_deku.Amount.of_int 10)
         ~ticket:ticket_1) in
  let state, _ = Core_deku.State.apply_user_operation state mock_hash op3 in
  (* fourth user operation as withdraw *)
  let op4 =
    Core_deku.User_operation.make ~source:deku_add_1
      (Build_operations.user_op_withdraw ~owner:tezos_add_1
         ~amount:(Core_deku.Amount.of_int 2)
         ~ticket:ticket_1) in
  let state, _ = Core_deku.State.apply_user_operation state mock_hash op4 in
  let new_storage = Core_deku.State.contract_storage state in
  (* CHECK the storage of the inital and the new one *)
  let old_contract =
    Core_deku.Contract_storage.get_contract ~address:contract_address
      init_storage
    |> Option.get in
  let new_contract =
    Core_deku.Contract_storage.get_contract ~address:contract_address
      new_storage
    |> Option.get in
  let test =
    [
      Alcotest.test_case "contract storage change" `Quick (fun () ->
          Alcotest.(check' bool)
            ~msg:"correct"
            ~expected:
              (Core_deku.Contract_vm.Contract.equal new_contract old_contract)
            ~actual:false);
    ] in
  (test, state)

(*********************************************************************************)
(* Build n times state *)

let init_state_n n () =
  let tezos_addresses = Build_usage.make_n_tezos_address n in
  let tickets = Build_usage.make_n_tickets n in
  let ops = Build_operations.deposits_n tezos_addresses tickets 10_000 in
  (* init state *)
  let state = Core_deku.State.empty in
  let tezos_operation_hash =
    init_tezos_operation_hash |> Tezos.Operation_hash.of_string |> Option.get
  in
  let payload =
    {
      Core_deku.Tezos_operation.tezos_operation_hash;
      internal_operations = ops;
    } in
  let tezos_operation = Core_deku.Tezos_operation.make payload in
  let state = Core_deku.State.apply_tezos_operation state tezos_operation in
  (state, tezos_addresses, tickets)

(* The number of n in init_state and build_state_n are the same *)

let build_state_n n () =
  (* todo *)
  let deku_addresses = Build_usage.make_n_address n in
  let deku_add_1 = List.nth deku_addresses 0 in
  (* deposits *)
  let init_state, _tezos_addresses, tickets = init_state_n n () in
  (* contract origination *)
  let code, storage = Build_operations.contract_vm in
  let initial_operation =
    Build_operations.user_op_contract_origination code storage in
  let op1 = Core_deku.User_operation.make ~source:deku_add_1 initial_operation in
  let mock_hash = Crypto.BLAKE2B.hash "mocked op hash" in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation init_state mock_hash op1 in
  let init_storage = Core_deku.State.contract_storage state in
  (* contract invocation payload same source *)
  let arg = Build_operations.contract_arg () in
  let user_op, contract_address =
    Build_operations.user_op_contract_invocation mock_hash arg in
  let op2 = Core_deku.User_operation.make ~source:deku_add_1 user_op in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation state mock_hash op2 in
  (* transfers with the same amount *)
  let amount = Core_deku.Amount.of_int 10 in
  (* NOTE: The source and destination coming from the same list.
     To make it different, the destination is a reverse list of source
  *)
  let sources = deku_addresses in
  let triples =
    let len_sources = List.length sources in
    let len_tickets = List.length tickets in
    if len_sources = len_tickets then
      List.fold_left2
        (fun result destination ticket ->
          (destination, amount, ticket) :: result)
        [] (List.rev sources) tickets
    else
      raise Build_operations.Length_not_equal in
  let states = Build_operations.n_transactions state sources triples mock_hash in
  (* CHECK state storage *)
  let new_storage = Core_deku.State.contract_storage state in
  let old_contract =
    Core_deku.Contract_storage.get_contract ~address:contract_address
      init_storage
    |> Option.get in
  let new_contract =
    Core_deku.Contract_storage.get_contract ~address:contract_address
      new_storage
    |> Option.get in
  let test =
    [
      Alcotest.test_case "contract storage change" `Quick (fun () ->
          Alcotest.(check' bool)
            ~msg:"correct"
            ~expected:
              (Core_deku.Contract_vm.Contract.equal new_contract old_contract)
            ~actual:false);
    ] in
  (test, states)

let build_state_3 () = build_state_n 3 ()

let build_state_10 () = build_state_n 10 ()
