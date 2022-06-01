(*********************************************************************************)
(* Tezos operation *)

let deposit_op ~destination ~ticket ~amount =
  Core_deku.Tezos_operation.Tezos_deposit
    { destination; ticket; amount = Core_deku.Amount.of_int amount }

(* Deposits n times, where the amount is the same *)

exception Length_not_equal

let deposits_n tezos_addresses tickets amount =
  let len_add = List.length tezos_addresses in
  let len_tick = List.length tickets in
  if len_add = len_tick then
    List.fold_left2
      (fun result tezos_address ticket ->
        let op = deposit_op ~destination:tezos_address ~ticket ~amount in
        op :: result)
      [] tezos_addresses tickets
  else
    raise Length_not_equal

(*********************************************************************************)
(* User operations *)

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

(* Contract invocation *)
let contract_arg () = Lambda_vm.Ast.(Int64 1L |> value_to_yojson)

let user_op_contract_invocation mock_hash arg =
  let contract_address =
    mock_hash |> Core_deku.Contract_address.of_user_operation_hash in
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

let n_transactions state sources triples mock_hash =
  let len_souces = List.length sources in
  let len_trips = List.length triples in
  if len_souces = len_trips then (* TODO check length *)
    List.fold_left2
      (fun _result source (destination, amount, ticket) ->
        (* todo: check the storage before and after *)
        (* each time transfer add it into state *)
        let user_op = user_op_transaction ~destination ~amount ~ticket in
        let op = Core_deku.User_operation.make ~source user_op in
        let state, _ = Core_deku.State.apply_user_operation state mock_hash op in
        state)
      Core_deku.State.empty sources triples
  else
    raise Length_not_equal

(* Withdraw where the:
   - owner: is the tezos address
*)
let user_op_withdraw ~owner ~amount ~ticket =
  Core_deku.User_operation.Tezos_withdraw { owner; amount; ticket }

let n_withdraw triples =
  List.fold_left
    (fun result (owner, amount, ticket) ->
      let user_op = user_op_withdraw ~owner ~amount ~ticket in
      user_op :: result)
    [] triples
