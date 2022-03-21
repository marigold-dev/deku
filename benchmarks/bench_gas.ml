open Core_bench
open Vm_utils

(* compile value *)
let test_compile_value_0 = bench_compile_value_n "" ~initial_gas:101 0L
let test_compile_value_4096 = bench_compile_value_n "" ~initial_gas:101 4096L

(* compile scripts *)
let ast_script = [%lambda_vm.script fun x -> x + 1L]

(*todo change to bench_compile, maybe move to utils?*)
let test_compile_script s ~initial_gas ~script =
  let name = "compile script " ^ s in
  Bench.Test.create ~name (fun () ->
      let _ = compile_script ~initial_gas script in
      ())
let test_compile_ast =
  test_compile_script "ast" ~initial_gas:501 ~script:ast_script
let test_compile_counter =
  test_compile_script "counter" ~initial_gas:14_747_900 ~script:counter_script

(* execute scripts *)
let test_execute_counter_0 =
  bench_execute_n 0L "counter_0" ~gas_value:101 ~gas_compile:14_747_900
    ~gas_exe:15_000_000 ~script:counter_script

let test_execute_counter_4096 =
  bench_execute_n 4096L "counter_4096" ~gas_value:101 ~gas_compile:14_747_900
    ~gas_exe:15_000_000 ~script:counter_script

let tests =
  [
    test_compile_value_0;
    test_compile_value_4096;
    test_compile_ast;
    test_compile_counter;
    test_execute_counter_0;
    test_execute_counter_4096;
  ]

let command = Bench.make_command tests