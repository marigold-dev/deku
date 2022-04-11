open Benchmark
open Lambda_vm
open Vm_utils
open Lambda_vm_tests
open Recursion_tests

let compile_value_f n f ~initial_gas =
  let n = f (Int64.of_int n) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let execute_f n ~script =
  let arg = compile_value_f n fac ~initial_gas:101 in
  let gas_compile = Gas.make ~initial_gas:10_000 in
  let ir = compile_exn gas_compile script in
  let gas = Gas.make ~initial_gas:1_000_000 in
  execute_exn gas arg ir

let bench_execute_recursion () =
  let list_bench =
    [
      ("factorial n=1", (fun () -> execute_f 1 ~script:factorial_script), ());
      ("fibonacci n=1", (fun () -> execute_f 1 ~script:fibonacci_script), ());
    ] in
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline "Benchmark recursion";
  tabulate res;

  let res = latencyN 20_000L list_bench in
  print_newline ();
  tabulate res

let benchmark_recursion () = bench_execute_recursion ()
