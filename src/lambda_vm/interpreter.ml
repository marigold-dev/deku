open Ir
open Checks

type error =
  (* interpreter bugs *)
  | Undefined_variable
  | Over_applied_primitives
  (* user program bugs *)
  | Value_is_not_pair
  | Value_is_not_int64
  | Value_is_not_function
  | Value_is_not_zero

exception Error of error
let raise error = raise (Error error)

module Pattern : sig
  type 'a t
  type nil = unit

  val script_result : (value * (nil * nil)) t
  val parse : value -> 'a t -> 'a
end = struct
  type nil = unit

  type 'a t =
    | Any : value t
    | Pair : 'left t * 'right t -> ('left * 'right) t
    | Nil : nil t

  let any = Any
  let pair left right = Pair (left, right)
  let nil = Nil

  let operations = nil
  let script_result = pair any (pair operations nil)

  let rec parse : type a. value -> a t -> a =
   fun value t ->
    match (t, value) with
    | Any, value -> value
    | Pair (left_t, right_t), V_pair (left_value, right_value) ->
      let left = parse left_value left_t in
      let right = parse right_value right_t in
      (left, right)
    | Pair _, (V_int64 _ | V_closure _ | V_primitive _) ->
      raise Value_is_not_pair
    | Nil, V_int64 0L -> ()
    | Nil, (V_int64 _ | V_pair _ | V_closure _ | V_primitive _) ->
      raise Value_is_not_zero
end

module Env = Map_with_cardinality.Make (Ident)

let burn_gas gas env code =
  (match code with
  | E_var _
  | E_app _ ->
    let cardinality = Env.cardinal env in
    Gas.burn_log2 gas ~cardinality
  | E_lam _
  | E_const _
  | E_prim _
  | E_if _
  | E_pair _
  | E_fst _
  | E_snd _ ->
    Gas.burn_constant gas);
  check_gas gas

let eval_prim prim ~arg ~args =
  let op1 f =
    let f value =
      match value with
      | V_int64 value -> V_int64 (f value)
      | V_pair _
      | V_closure _
      | V_primitive _ ->
        raise Value_is_not_int64 in
    match args with
    | [] -> f arg
    | _ -> raise Over_applied_primitives in

  let op2 f =
    (* error only happens after both are applied *)
    let f left right =
      match (left, right) with
      | V_int64 left, V_int64 right -> V_int64 (f left right)
      | ( (V_pair _ | V_int64 _ | V_closure _ | V_primitive _),
          (V_pair _ | V_int64 _ | V_closure _ | V_primitive _) ) ->
        raise Value_is_not_int64 in
    match args with
    | [] -> V_primitive { args = [arg]; prim }
    | [left] -> f left arg
    | _ -> raise Over_applied_primitives in

  let op2_shift f =
    let f left right = f left (Int64.to_int right) in
    op2 f in
  match prim with
  | P_neg -> op1 Int64.neg
  | P_add -> op2 Int64.add
  | P_sub -> op2 Int64.sub
  | P_mul -> op2 Int64.mul
  | P_div -> op2 Int64.div
  | P_rem -> op2 Int64.rem
  | P_and -> op2 Int64.logand
  | P_or -> op2 Int64.logor
  | P_xor -> op2 Int64.logxor
  | P_lsl -> op2_shift Int64.shift_left
  | P_lsr -> op2_shift Int64.shift_right_logical
  | P_asr -> op2_shift Int64.shift_right

(* TODO: gas must be ref *)
let rec eval ~stack gas env code =
  let eval_call env code = eval ~stack:(stack - 1) gas env code in
  let eval_jump env code = eval ~stack gas env code in

  check_stack ~stack;
  burn_gas gas env code;

  match code with
  | E_var var -> (
    (* TODO: gas cost for this *)
    (* TODO: shuold this be a hashtbl? Or an array? *)
    match Env.find var env with
    | Some value -> value
    | None ->
      (* TODO: could we eliminate this using GADTs? *) raise Undefined_variable)
  | E_lam (param, body) -> V_closure { env; param; body }
  | E_app (funct, arg) -> (
    let funct = eval_call env funct in
    let arg = eval_call env arg in
    match funct with
    | V_pair _
    | V_int64 _ ->
      raise Value_is_not_function
    | V_closure { env; param; body } ->
      let env = Env.add param arg env in
      eval_jump env body
    | V_primitive { args; prim } -> eval_prim prim ~arg ~args)
  | E_const value -> V_int64 value
  | E_prim prim -> V_primitive { args = []; prim }
  | E_if { condition; then_; else_ } -> (
    let condition = eval_call env condition in
    match condition with
    | V_int64 0L -> eval_jump env else_
    | V_int64 _ -> eval_jump env then_
    | V_pair _
    | V_closure _
    | V_primitive _ ->
      raise Value_is_not_int64)
  | E_pair (left, right) ->
    let left = eval_call env left in
    let right = eval_call env right in
    V_pair (left, right)
  | E_fst pair ->
    let pair = eval_call env pair in
    let left, _right =
      match pair with
      | V_pair (left, right) -> (left, right)
      | V_int64 _
      | V_closure _
      | V_primitive _ ->
        raise Value_is_not_pair in
    left
  | E_snd pair ->
    let pair = eval_call env pair in
    let _left, right =
      match pair with
      | V_pair (left, right) -> (left, right)
      | V_closure _
      | V_int64 _
      | V_primitive _ ->
        raise Value_is_not_pair in
    right
let eval gas env code =
  let stack = max_stack_depth in
  eval ~stack gas env code

type script_result = {
  storage : Ir.value;
  operations : unit;
}

let execute gas ~arg script =
  let { param; code } = script in
  let env = Env.add param arg Env.empty in
  let output = eval gas env code in
  let storage, (operations, ()) = Pattern.(parse output script_result) in
  { storage; operations }

let execute gas ~arg script =
  try Ok (execute gas ~arg script) with
  | Error error -> Error error
