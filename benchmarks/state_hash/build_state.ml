(*********************************************************************************)
(* Initial state with deposits *)
open Build_usage

let init_tezos_operation_hash =
  "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

let check_balance address ticket expected ledger =
  let amount = Core_deku.Amount.of_int expected in
  let balance = Core_deku.Ledger.balance address ticket ledger in
  amount = balance

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
  (* Check deposit balance *)
  (* Convert tezos-addresses into deku-addresses to be able to use it
      in Deku as it is deposit into this address.
     - If I create a new Deku address, the deposit is 0.
  *)
  let deku_add_1 =
    tezos_add_1
    |> Tezos.Address.to_string
    |> Crypto.Key_hash.of_string
    |> Option.get in
  let deku_add_2 =
    tezos_add_2
    |> Tezos.Address.to_string
    |> Crypto.Key_hash.of_string
    |> Option.get in

  (* CHECK *)
  (*let _ =
      let balance =
        Core_deku.Ledger.balance deku_add_1 ticket_1
          (Core_deku.State.ledger state) in
      Printf.printf "balance deposit 1: %i \n" (Core_deku.Amount.to_int balance)
    in*)
  ( state,
    (tezos_add_1, tezos_add_2),
    (deku_add_1, deku_add_2),
    (ticket_1, ticket_2) )

(*******************************************************************************)
(* Build basic case for state *)

let build_state () =
  let ( init_state,
        (tezos_add_1, _tezos_add_2),
        (deku_add_1, deku_add_2),
        (ticket_1, _) ) =
    init_state () in
  let code, storage = Build_operations.contract_vm in
  let initial_operation =
    Build_operations.user_op_contract_origination code storage in
  (* first user operation as contract origination,
     source is the destination address of tezos_operation
  *)
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
  (*let _ =
      let balance =
        Core_deku.Ledger.balance deku_add_2 ticket_1
          (Core_deku.State.ledger state) in
      Printf.printf "balance basic here: %i \n" (Core_deku.Amount.to_int balance)
    in*)
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
  (* Convert tezos_addresses to Deku addresses *)
  let deku_addresses = Build_usage.make_n_deku_addresses tezos_addresses in
  let _ =
    List.fold_left2
      (fun _ deku_add ticket ->
        let balance =
          Core_deku.Ledger.balance deku_add ticket
            (Core_deku.State.ledger state) in
        Printf.printf "balance deposit: %i \n" (Core_deku.Amount.to_int balance))
      () deku_addresses tickets in
  (state, tezos_addresses, deku_addresses, tickets)

(* The number of n in init_state and build_state_n are the same *)

let build_state_n n () =
  (* deposits *)
  let init_state, _tezos_addresses, deku_addresses, tickets =
    init_state_n n () in
  (* contract origination *)
  let code, storage = Build_operations.contract_vm in
  let deku_add_1 = List.nth deku_addresses 0 in
  let initial_operation =
    Build_operations.user_op_contract_origination code storage in
  let op1 = Core_deku.User_operation.make ~source:deku_add_1 initial_operation in
  let mock_hash = Crypto.BLAKE2B.hash "mocked op hash" in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation init_state mock_hash op1 in
  let _init_storage = Core_deku.State.contract_storage state in
  (* contract invocation payload same source *)
  let arg = Build_operations.contract_arg () in
  let user_op, _contract_address =
    Build_operations.user_op_contract_invocation mock_hash arg in
  let op2 = Core_deku.User_operation.make ~source:deku_add_1 user_op in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation state mock_hash op2 in
  (* transfers with the same amount *)
  let amount = Core_deku.Amount.of_int 10 in
  (* NOTE: The source and destination coming from the same list.
     To make it different, the destination is a reverse list of source
  *)
  (* TODO: check balance in the ledger
     Ledger.balance address ticket t
  *)
  let sources = deku_addresses in
  let destinations = List.rev sources in
  let triples =
    let len_sources = List.length sources in
    let len_tickets = List.length tickets in
    if len_sources = len_tickets then
      List.fold_left2
        (fun result destination ticket ->
          (destination, amount, ticket) :: result)
        [] destinations tickets
    else
      raise Build_operations.Length_not_equal in
  let ops = Build_operations.n_transactions' sources triples in
  let ops_len = List.length ops in
  let () =
    for i = 0 to ops_len - 1 do
      let op = List.nth ops i in
      let state, _ = Core_deku.State.apply_user_operation state mock_hash op in
      let hash = Core_deku.State.hash state in 
      let _ =
        List.fold_left2
          (fun _ des ticket ->
            let balance =
              Core_deku.Ledger.balance des ticket
                (Core_deku.State.ledger state) in
            let _ =
              Printf.printf "balance %i - ops len: %i \n" (Core_deku.Amount.to_int balance)
            ops_len
            in
            ())()
          destinations tickets in
      ignore hash
    done in
()

   (* 
  let states = Build_operations.n_transactions state sources triples mock_hash in
  (* CHECK balance *)
  (*let _ =
    (* check balance of sources and tickets *)
    let () =
      List.fold_left2
        (fun _ source ticket ->
          let balance =
            Core_deku.Ledger.balance source ticket
              (Core_deku.State.ledger states) in
          Printf.printf " transfer source: %i \n"
            (Core_deku.Amount.to_int balance))
        () sources tickets in
    (* Check balance of destination and tickets *)
    let () =
      List.fold_left2
        (fun _ des ticket ->
          let balance =
            Core_deku.Ledger.balance des ticket
              (Core_deku.State.ledger states) in
          Printf.printf " transfer dest: %i \n"
            (Core_deku.Amount.to_int balance))
        () destinations tickets in
    () in*)
  (* CHECK state storage *)
  let new_storage = Core_deku.State.contract_storage states in
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
  (test, states)*)

let build_state_3 () = build_state_n 3 ()

let _build_state_10 () = build_state_n 10 ()
