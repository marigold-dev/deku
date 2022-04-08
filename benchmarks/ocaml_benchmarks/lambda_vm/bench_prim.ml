open Lambda_vm
open Vm_utils
open Benchmark

let prim prim = Ast.Prim prim

(* Primivites with 1 parameter *)
let script_op1 prim =
  [%lambda_vm.script fun param -> ([%e prim] param, (0L, 0L))]

(* negation *)
let script_neg = script_op1 (prim Neg)

(* negative n : Int64 (Int64.neg n),
   give an initial gas as 200 *)
let compile_value_neg_lib n =
  let n = Int64.(neg (Int64.of_int n)) in
  let gas = Gas.make ~initial_gas:200 in
  compile_value_exn gas (Int64 n)

(**********************************************************)
(* TODO add this inside the throughputN or latencyN *)

let () =
  (* benchmark for throughputN repeat 5 times, in 10 CPU seconds *)
  let res = 
    throughputN ~repeat:5 10
    [
      ("n=1", compile_value_neg_lib, 1)
    ]
  in 
  print_newline ();
  tabulate res;

  (* benchmark for latencyN:
    - 20000L: latency for 2000 iterations *)
  let res =
    latencyN 20000L
    [("n=1", compile_value_neg_lib, 1) ]
  in 
  print_newline ();
  tabulate res