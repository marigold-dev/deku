(*
  A Deku state is of:
  type t =
  {
    ledger : Ledger.t;
    contract_storage: Contract_storage.t
  }
*)

(* TODO : not complete yet *)
(* Reference:
   https://github.com/marigold-dev/deku/blob/quyen%40copy_swerve_benchmarks/tests/contracts/test_invocation.ml
*)

(* Build initial state with the tezos operations *)
let init_tezos_operation_hash =
  "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"

let init_state ?(init_amount = 10_000) () : State.t =
  let destination = Build_ledger.make_tezos_address () in
  let ticket = Build_ledger.make_ticket () in
  let internal_operation =
    Tezos_operation.Tezos_deposit
      { destination; ticket; amount = Amount.of_int init_amount } in
  let state = State.empty in
  let tezos_operation_hash =
    init_tezos_operation_hash
    |> Tezos.Operation_hash.of_raw_string
    |> Option.get in
  let payload =
    {
      Tezos_operation.tezos_operation_hash;
      internal_operations = [internal_operation];
    } in
  let tezos_operation = Tezos_operation.make payload in
  (* make tezos address as a source address for this operation *)
  let make_address =
    destination
    |> Tezos.Address.to_string
    |> Address.of_string
    |> Option.map Address.to_key_hash
    |> Option.join
    |> Option.get in
  let state = State.apply_tezos_operation state tezos_operation in
  (state, make_address)

(* Build user init operation as a contract origination where 
    we choose the Origination_payload as Lambda type
*)
let user_operation_contract_origination () : User_operation.initial_operation =
  let script =
    [%lamda_vm.script
      fun param ->
        ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]
  in
  let value = Lambda_vm.(Ast.int64 1L) in
  let code = Lambda_vm.Ast.script_to_yojson script in
  let storage = Lambda_vm.Ast.value_to_yoson value in
  let payload =
    Contract_vm.Origination_payload.lamda_of_yojson ~code ~storage
    |> Result.get_ok in
  User_operation.Contract_origination payload

(* TODO other user operation like transfer, withdraw, etc. *)

(* Build state *)
let build_state () : State.t =
  let init_state, source = init_state () in
  let initial_operation = user_operation_contract_origination () in
  let user_operation = User_operation.make ~source initial_operation in
  let state, _ = State.apply_user_operation init_state user_operation in
  state
