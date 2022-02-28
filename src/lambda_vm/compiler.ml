open Ast
open Ir
open Checks

module String_map = Map.Make (String)

type error = (* user program bugs *)
  | Undefined_variable
exception Error of error
let raise error = raise (Error error)

let compile_prim prim =
  match prim with
  | Neg -> P_neg
  | Add -> P_add
  | Sub -> P_sub
  | Mul -> P_mul
  | Div -> P_div
  | Rem -> P_rem
  | And -> P_and
  | Or -> P_or
  | Xor -> P_xor
  | Lsl -> P_lsl
  | Lsr -> P_lsr
  | Asr -> P_asr

module Vars = Map_with_cardinality.Make (String)

let burn_gas gas vars code =
  (match code with
  | Var _
  | Lam _ ->
    let cardinality = Vars.cardinal vars in
    Gas.burn_log2 gas ~cardinality
  | App _
  | Const _
  | Prim _
  | If _
  | Pair _
  | Fst _
  | Snd _ ->
    Gas.burn_constant gas);
  check_gas gas

let rec compile_expr ~stack gas next_ident vars code =
  let stack = stack - 1 in

  check_stack ~stack;
  burn_gas gas vars code;

  let compile_expr vars code = compile_expr ~stack gas next_ident vars code in

  match code with
  | Var var -> (
    match Vars.find var vars with
    | Some ident -> E_var ident
    | None -> raise Undefined_variable)
  | Lam (var, body) ->
    let ident =
      let ident = !next_ident in
      next_ident := Ident.next ident;
      ident in
    let vars = Vars.add var ident vars in
    let body = compile_expr vars body in
    E_lam (ident, body)
  | App (funct, body) ->
    let funct = compile_expr vars funct in
    let body = compile_expr vars body in
    E_app (funct, body)
  (* prims *)
  | Const value -> E_const value
  | Prim prim ->
    let prim = compile_prim prim in
    E_prim prim
  (* branching *)
  | If { condition; then_; else_ } ->
    let condition = compile_expr vars condition in
    let then_ = compile_expr vars then_ in
    let else_ = compile_expr vars else_ in
    E_if { condition; then_; else_ }
  (* memory *)
  | Pair (left, right) ->
    let left = compile_expr vars left in
    let right = compile_expr vars right in
    E_pair (left, right)
  | Fst pair ->
    let pair = compile_expr vars pair in
    E_fst pair
  | Snd pair ->
    let pair = compile_expr vars pair in
    E_snd pair
let compile_expr gas next_ident vars code =
  let stack = max_stack_depth in
  compile_expr ~stack gas next_ident vars code

let compile gas script =
  let Ast.{ param; code } = script in

  let param_ident, next_ident =
    let param_ident = Ident.initial in
    let next_ident = Ident.next param_ident in
    (param_ident, ref next_ident) in
  let vars = Vars.add param param_ident Vars.empty in

  let code = compile_expr gas next_ident vars code in
  { param = param_ident; code }

let compile gas script =
  try Ok (compile gas script) with
  | Error error -> Error error

let burn_gas gas =
  Gas.burn_constant gas;
  check_gas gas

let rec compile_value ~stack gas value =
  let compile_value value = compile_value ~stack:(stack - 1) gas value in

  check_stack ~stack;
  burn_gas gas;

  match value with
  | Int64 value -> V_int64 value
  | Pair (left, right) ->
    let left = compile_value left in
    let right = compile_value right in
    V_pair (left, right)

let compile_value gas value =
  let stack = max_stack_depth in
  compile_value ~stack gas value
let compile_value gas value =
  try Ok (compile_value gas value) with
  | Error error -> Error error
