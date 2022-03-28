open Ast
open Ir
open Checks

module String_map = Map.Make (String)

type error =
  (* user program bugs *)
  [ `Undefined_variable
  | Gas.error
  | Checks.error ]
[@@deriving show]

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
  | Land -> P_land
  | Lor -> P_lor
  | Lxor -> P_lxor
  | Lsl -> P_lsl
  | Lsr -> P_lsr
  | Asr -> P_asr
  | Fst -> P_fst
  | Snd -> P_snd

module Vars = Map_with_cardinality.Make (struct
  include String
  let to_yojson t = `String t
  let of_yojson = function
    | `String t -> Ok t
    | Yojson.Safe.(#t) -> Error "failure parsing Vars.t"
end)

let burn_gas gas vars code =
  match code with
  | Var _
  | Lam _ ->
    let cardinality = Vars.cardinal vars in
    Gas.burn_log2 gas ~cardinality
  | App _
  | Const _
  | Prim _
  | If _
  | Pair _ ->
    Gas.burn_constant gas

let rec compile_expr ~stack gas next_ident vars code =
  let stack = stack - 1 in

  check_stack ~stack;
  burn_gas gas vars code;

  let compile_expr vars code = compile_expr ~stack gas next_ident vars code in

  match code with
  | Var var -> (
    match Vars.find var vars with
    | Some ident -> E_var ident
    | None -> raise `Undefined_variable)
  | Lam (var, body) ->
    let ident =
      let ident = !next_ident in
      next_ident := Ident.next ident;
      ident in
    let vars = Vars.add var ident vars in
    let body = compile_expr vars body in
    E_lam (ident, body)
  | App { funct; arg } ->
    let funct = compile_expr vars funct in
    let arg = compile_expr vars arg in
    E_app { funct; arg }
  (* prims *)
  | Const value -> E_const value
  | Prim prim ->
    let prim = compile_prim prim in
    E_prim prim
  (* branching *)
  | If { predicate; consequent; alternative } ->
    let predicate = compile_expr vars predicate in
    let consequent = compile_expr vars consequent in
    let alternative = compile_expr vars alternative in
    E_if { predicate; consequent; alternative }
  (* memory *)
  | Pair { first; second } ->
    let first = compile_expr vars first in
    let second = compile_expr vars second in
    E_pair { first; second }

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
  | Checks.Out_of_stack -> Error `Out_of_stack
  | Gas.Out_of_gas -> Error `Out_of_gas
let burn_gas gas = Gas.burn_constant gas

let rec compile_value ~stack gas value =
  let compile_value value = compile_value ~stack:(stack - 1) gas value in

  check_stack ~stack;
  burn_gas gas;

  match value with
  | Int64 value -> V_int64 value
  | Pair (first, second) ->
    let first = compile_value first in
    let second = compile_value second in
    V_pair { first; second }

let compile_value gas value =
  let stack = max_stack_depth in
  compile_value ~stack gas value

let compile_value gas value =
  try Ok (compile_value gas value) with
  | Error error -> Error error
  | Checks.Out_of_stack -> Error `Out_of_stack
  | Gas.Out_of_gas -> Error `Out_of_gas
