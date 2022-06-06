exception Length_not_equal

let deposits_n tezos_addresses tickets amount =
  let len_add = List.length tezos_addresses in
  let len_tick = List.length tickets in
  if len_add = len_tick then
    List.fold_left2
      (fun result tezos_add ticket_id ->
        let op =
          Core_deku.Tezos_operation.Tezos_deposit
            {
              destination = tezos_add;
              ticket = ticket_id;
              amount = Core_deku.Amount.of_int amount;
            } in
        op :: result)
      [] tezos_addresses tickets
  else
    raise Length_not_equal

let print_deku_address deku_add =
  Printf.printf "Deku_add: %s - " (Crypto.Key_hash.to_string deku_add)

let print_balance add ticket ledger =
  let balance = Core_deku.Ledger.balance add ticket ledger in
  print_deku_address add;
  Printf.printf "amount: %i \n" (Core_deku.Amount.to_int balance)

let expect_balance address ticket expected ledger =
  let balance = Core_deku.Ledger.balance address ticket ledger in
  Core_deku.Amount.of_int expected = balance

let n_transactions sources triples =
  let len_sources = List.length sources in
  let len_triples = List.length triples in
  (* Sending from source to dest with the
     same amount and store in a ticket *)
  if len_sources = len_triples then
    List.fold_left2
      (fun result source (dest, amount, ticket) ->
        let user_op =
          Core_deku.User_operation.Transaction
            { destination = dest; amount; ticket } in
        let op = Core_deku.User_operation.make ~source user_op in
        op :: result)
      [] sources triples
  else
    raise Length_not_equal

let init_tezos_operation_hash =
  "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

exception Balance_not_equal

let build_init_state_n n () =
  (* build n tezos addresses, tickets,
     then deposits the same amount to those tickets *)
  let tezos_addresses = Build_usage.make_n_tezos_address n in
  let tickets = Build_usage.make_n_tickets n in
  let amount = 10_000 in
  let ops = deposits_n tezos_addresses tickets amount in
  (* initial state *)
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
  (* deku_addresses: It is a revert list *)
  let deku_addresses = Build_usage.make_n_deku_addresses tezos_addresses in
  (* Check deposit balance *)
  let () =
    List.iter2
      (fun deku_address ticket ->
        let amount' =
          Core_deku.Ledger.balance deku_address ticket
            (Core_deku.State.ledger state) in
        let b = Core_deku.Amount.to_int amount' = amount in
        if b then () else raise Balance_not_equal)
      (List.rev deku_addresses) tickets in
  (state, tezos_addresses, List.rev deku_addresses, tickets)

(* Contract origination para *)
let contract_vm =
  let script =
    [%lambda_vm.script
      fun param ->
        ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]
  in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let storage = Lambda_vm.Ast.value_to_yojson value in
  let code = Lambda_vm.Ast.script_to_yojson script in
  (code, storage)

let user_op_contract_origination code storage =
  let payload =
    Core_deku.Contract_vm.Origination_payload.lambda_of_yojson ~code ~storage
    |> Result.get_ok in
  Core_deku.User_operation.Contract_origination payload

let build_state_n n () =
  let init_state, _tezos_addresses, deku_addresses, tickets =
    build_init_state_n n () in
  (* originate contract, the source is the first address *)
  let code, storage = contract_vm in
  let deku_add_1 = List.nth deku_addresses 0 in
  let deku_add_2 = List.nth deku_addresses 1 in
  let deku_add_3 = List.nth deku_addresses 2 in
  let initial_operation = user_op_contract_origination code storage in
  let op1 = Core_deku.User_operation.make ~source:deku_add_1 initial_operation in
  let mock_hash = Crypto.BLAKE2B.hash "mocked op hash" in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation init_state mock_hash op1 in
  let op2 = Core_deku.User_operation.make ~source:deku_add_2 initial_operation in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation state mock_hash op2 in
  let op3 = Core_deku.User_operation.make ~source:deku_add_3 initial_operation in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation state mock_hash op3 in

  (* transfer *)
  let amount = Core_deku.Amount.of_int 10 in
  let sources = deku_addresses in
  let destinations = List.rev deku_addresses in
  let build_triples =
    let len_dest = List.length destinations in
    let len_tickets = List.length tickets in
    if len_dest = len_tickets then
      List.fold_left2
        (fun result dest ticket -> (dest, amount, ticket) :: result)
        [] destinations tickets
    else
      raise Length_not_equal in
  let ops = n_transactions sources build_triples in
  let len_ops = List.length ops in
  let () =
    for i = 0 to len_ops - 1 do
      let op = List.nth ops i in
      let state, _ = Core_deku.State.apply_user_operation state mock_hash op in
      let hash = Core_deku.State.hash state in
      let () =
        List.iter2
          (fun des ticket ->
            let balance =
              Core_deku.Ledger.balance des ticket (Core_deku.State.ledger state)
            in
            Printf.printf "i %i - balance %i - address: %s \n" i
              (Core_deku.Amount.to_int balance)
              (Crypto.Key_hash.to_string des);
            ())
          sources tickets in
      ignore hash
    done in
  let _test =
    [
      Alcotest.test_case "balance " `Quick (fun () ->
          Alcotest.(check' bool) ~msg:"correct" ~expected:(1 = 1) ~actual:true);
    ] in
  ()

let build_state () = build_state_n 4 ()
