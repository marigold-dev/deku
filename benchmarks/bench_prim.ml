open Core_bench
open Lambda_vm
open Vm_utils

let prim prim = Ast.Prim prim

(* Primivites with 1 parameter *)
let script_op1 prim =
  [%lambda_vm.script fun param -> ([%e prim] param, (0L, 0L))]
let script_neg = script_op1 (prim Neg)

let compile_value_neg_lib n ~initial_gas =
  let n = Int64.(neg (Int64.of_int n)) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let test_compile_value_neg_lib =
  Bench.Test.create_indexed ~name:"compile value neg lib" ~args:[0; 1; 2]
    (fun n ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_neg_lib n ~initial_gas:200 in
          ()))

let test_execute_neg_lib =
  Bench.Test.create_indexed ~name:"execute neg lib" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_neg_lib n ~initial_gas:200 in
          let gas_compile = Gas.make ~initial_gas:1501 in
          let ir = compile_exn gas_compile script_neg in
          let gas = Gas.make ~initial_gas:1501 in
          let _ = execute_exn gas arg ir in
          ()))

(* negative n: Int64 n *)
let compile_value_neg n ~initial_gas =
  let n = Int64.of_int n in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let test_compile_value_neg =
  Bench.Test.create_indexed ~name:"compile value neg" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_neg n ~initial_gas:200 in
          ()))
let test_execute_neg =
  Bench.Test.create_indexed ~name:"execute neg" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_neg n ~initial_gas:200 in
          let gas_compile = Gas.make ~initial_gas:1501 in
          let ir = compile_exn gas_compile script_neg in
          let gas = Gas.make ~initial_gas:1501 in
          let _ = execute_exn gas arg ir in
          ()))

(* Primitives with 2 parameteres *)
let script_op2 prim =
  [%lambda_vm.script fun param -> ([%e prim] (fst param) (snd param), (0L, 0L))]

(* addition *)
let script_add = script_op2 (prim Add)

(* subtraction *)
let script_sub = script_op2 (prim Sub)

let script_mul = script_op2 (prim Mul)

let script_div = script_op2 (prim Div)

let script_rm = script_op2 (prim Rem)

let script_and = script_op2 (prim Land)

let script_or = script_op2 (prim Lor)

let script_xor = script_op2 (prim Lxor)

let script_lsl = script_op2 (prim Lsl)

let script_lsr = script_op2 (prim Lsr)

let script_asr = script_op2 (prim Asr)

let compile_value_sum = Bench.Test.create ~name:"" (fun () -> ())

let tests =
  [
    test_compile_value_neg;
    test_compile_value_neg_lib;
    test_execute_neg_lib;
    test_execute_neg;
  ]

let command = Bench.make_command tests