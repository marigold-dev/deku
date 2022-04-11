open Lambda_vm

let prim prim = Ast.Prim prim

(* Primivites with 1 parameter *)
let script_op1 prim =
  [%lambda_vm.script fun param -> ([%e prim] param, (0L, 0L))]

(* negation *)
let script_neg = script_op1 (prim Neg)

(* Primitives with 2 parameteres *)
let script_op2 prim =
  [%lambda_vm.script fun param -> ([%e prim] (fst param) (snd param), (0L, 0L))]

(* addition *)

(* Note: I cannot parse the execute of libary because the compile value of
    Int64.add is (Int64 value) and not a (Pair value) while the script is a pair
    value *)

let script_add = script_op2 (prim Add)

(* subtraction *)
let script_sub = script_op2 (prim Sub)

(* Multiplication *)
let script_mul = script_op2 (prim Mul)

(* Division *)
let script_div = script_op2 (prim Div)

(* Remainder *)
let script_rem = script_op2 (prim Rem)

(* Bitwise logical and *)
let script_land = script_op2 (prim Land)

(* Bitwise logical or *)
let script_lor = script_op2 (prim Lor)

(* Bitwise logical exclusive or *)
let script_lxor = script_op2 (prim Lxor)

(* Shift left *)
let script_lsl = script_op2 (prim Lsl)

(* Shift right logical *)
let script_lsr = script_op2 (prim Lsr)

(* Shift right *)
let script_asr = script_op2 (prim Asr)
