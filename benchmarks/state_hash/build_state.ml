(* Build initial state with the tezos operations *)

(* The address of a destination of a withdraw must be a tezos_address *)
let make_tezos_address () =
  let open Tezos in
  let _key, address = Crypto.Ed25519.generate () in
  let hash = Crypto.Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

let make_ticket ?ticketer ?data () =
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Crypto.Random.generate 20
        |> Cstruct.to_string
        |> Crypto.BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Crypto.Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  { ticketer; data }

(*******************************************************************************)
(* Tezos operation *)

let init_tezos_operation_hash =
  "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

(* TODO: make the list of tezos_address; tickets, and n deposits *)
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
  (* convert Tezos addresses into Deku addresses *)
  let deku_add_1 : Crypto.Key_hash.t =
    tezos_add_1
    |> Tezos.Address.to_string
    |> Core_deku.Address.of_string
    |> Option.map Core_deku.Address.to_key_hash
    |> Option.join
    |> Option.get in
  let deku_add_2 : Crypto.Key_hash.t =
    tezos_add_2
    |> Tezos.Address.to_string
    |> Core_deku.Address.of_string
    |> Option.map Core_deku.Address.to_key_hash
    |> Option.join
    |> Option.get in
  let state = Core_deku.State.apply_tezos_operation state tezos_operation in
  ( state,
    (tezos_add_1, tezos_add_2),
    (deku_add_1, deku_add_2),
    (ticket_1, ticket_2) )

(*******************************************************************************)
(* User operations *)

let user_op_contract_origination () : Core_deku.User_operation.initial_operation
    =
  let script =
    [%lambda_vm.script
      fun param ->
        ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]
  in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let code = Lambda_vm.Ast.script_to_yojson script in
  let storage = Lambda_vm.Ast.value_to_yojson value in
  let payload =
    Core_deku.Contract_vm.Origination_payload.lambda_of_yojson ~code ~storage
    |> Result.get_ok in
  Core_deku.User_operation.Contract_origination payload

(* The contract_address is a hash of the contract origination operation *)
let user_op_contract_invocation user_operation =
  let contract_address =
    user_operation.Core_deku.User_operation.hash
    |> Core_deku.Contract_address.of_user_operation_hash in
  let arg = Lambda_vm.Ast.(Int64 1L |> value_to_yojson) in
  let invocation_payload =
    Core_deku.Contract_vm.Invocation_payload.lambda_of_yojson ~arg
    |> Result.get_ok in
  ( Core_deku.User_operation.Contract_invocation
      { to_invoke = contract_address; argument = invocation_payload },
    contract_address )

(* Transaction
   - destination address is a deku_address
   - ticket can be store in the ticket that deposited(s)
*)
let user_op_transaction ~destination ~amount ~ticket =
  Core_deku.User_operation.Transaction { destination; amount; ticket }

(* Withdraw where the:
   - owner: is the tezos address
*)
let user_op_withdraw ~owner ~amount ~ticket =
  Core_deku.User_operation.Tezos_withdraw { owner; amount; ticket }

(*******************************************************************************)
(* Build state:
   - Add a lot of user_operation(s) and then apply each of them to build the
   size of state
*)
let build_state () =
  let ( init_state,
        (tezos_add_1, _tezos_add_2),
        (deku_add_1, deku_add_2),
        (ticket_1, _) ) =
    init_state () in
  let initial_operation = user_op_contract_origination () in
  (* first user operation as contract origination,
     source is the destination address of tezos_operation
  *)
  let op1 = Core_deku.User_operation.make ~source:deku_add_1 initial_operation in
  let state, _receipt_option =
    Core_deku.State.apply_user_operation init_state op1 in
  let init_storage = Core_deku.State.contract_storage state in
  (* second user operation as contract invocation payload same source *)
  let user_op, contract_address = user_op_contract_invocation op1 in
  let op2 = Core_deku.User_operation.make ~source:deku_add_1 user_op in
  let state, _receipt_option = Core_deku.State.apply_user_operation state op2 in
  (* third user operation as transfer same source *)
  let op3 =
    Core_deku.User_operation.make ~source:deku_add_1
      (user_op_transaction ~destination:deku_add_2
         ~amount:(Core_deku.Amount.of_int 10)
         ~ticket:ticket_1) in
  let state, _ = Core_deku.State.apply_user_operation state op3 in
  (* fourth user operation as withdraw *)
  let op4 =
    Core_deku.User_operation.make ~source:deku_add_1
      (user_op_withdraw ~owner:tezos_add_1
         ~amount:(Core_deku.Amount.of_int 2)
         ~ticket:ticket_1) in
  let state, _ = Core_deku.State.apply_user_operation state op4 in
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
