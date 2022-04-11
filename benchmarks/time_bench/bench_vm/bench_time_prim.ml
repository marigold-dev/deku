open Lambda_vm
open Vm_utils
open Lambda_vm_tests
open Primitives_tests
open Benchmark

(* negative n : Int64 (Int64.neg n),
   give an initial gas as 200 *)
let compile_value_neg_lib n ~initial_gas =
  let n = Int64.(neg (Int64.of_int n)) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let compile_neg_lib script =
  let gas_compile = Gas.make ~initial_gas:1501 in
  compile_exn gas_compile script

let execute_neg_lib n script =
  let arg = compile_value_neg_lib n ~initial_gas:200 in
  let gas_compile = Gas.make ~initial_gas:1501 in
  let ir = compile_exn gas_compile script in
  let gas = Gas.make ~initial_gas:1501 in
  execute_exn gas arg ir

(**********************************************************)
(* TODO: optimizing later after being merged/or after done
   writing test, share the same code with core_bench.
   TODO: change the repeat times, iterations in latencyN, etc.
   TODO: in readme, write details about the senario and input values
   for these benchmarks: n, initial gas, etc.
*)

(* benchmark compile value for negative *)
let bench_compile_value_neg s () =
  let list_bench =
    [
      ( "compile value: n=0",
        (fun () -> compile_value_neg_lib ~initial_gas:200 0),
        () );
      ( "compile value: n=1",
        (fun () -> compile_value_neg_lib ~initial_gas:200 1),
        () );
      ( "compile value: n=2",
        (fun () -> compile_value_neg_lib ~initial_gas:200 2),
        () );
    ] in
  (* benchmark for throughputN repeat 5 times, in 10 CPU seconds *)
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  (* benchmark for latencyN:
     - 20000L: latency for 2000 iterations *)
  let res = latencyN 20000L list_bench in
  print_newline ();
  tabulate res

(* benchmark compile for negative *)
let bench_compile_neg s () =
  let list_bench =
    [("compile: negative", (fun () -> compile_neg_lib script_neg), ())] in
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  let res = latencyN 20000L list_bench in
  print_newline ();
  tabulate res

(* benchmark iterpreter for negative *)

let bench_execute_neg s () =
  let list_bench =
    [
      ("execute: n=0", (fun () -> execute_neg_lib 0 script_neg), ());
      ("execute: n=1", (fun () -> execute_neg_lib 1 script_neg), ());
      ("execute: n=2", (fun () -> execute_neg_lib 2 script_neg), ());
    ] in
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  let res = latencyN 20000L list_bench in
  print_newline ();
  tabulate res
