open Core_bench
open Vm_utils

(* compile value *)
let test_compile_value_0 = bench_compile_value "" ~initial_gas:101 0L

let test_compile_value_4096 = bench_compile_value "" ~initial_gas:101 4096L

(* compile scripts *)
let ast_script = [%lambda_vm.script fun x -> x + 1L]

let test_compile_ast =
  bench_compile_script "ast" ~initial_gas:501 ~script:ast_script

let test_compile_counter =
  bench_compile_script "counter" ~initial_gas:14_747_900 ~script:counter_script

(* execute scripts *)

(* TODO execute ast script *)

let test_execute_counter_4096 =
  bench_execute_exn 4096L "counter_4096" ~gas_value:101 ~gas_compile:14_747_900
    ~gas_exe:15_000_000 ~script:counter_script

(* Bench execute for different initial gas value (gas execute, gas compile)
   - start: counter_script value 0L initial gas value:101
*)

let bench_exe_counter_gas s ~gas_value =
  let s = "counter_gas_" ^ s in
  bench_execute_exn 0L s ~gas_value ~gas_compile:14_747_900 ~gas_exe:15_000_000
    ~script:counter_script

let test_execute_counter_0 = bench_exe_counter_gas "0" ~gas_value:101

let test_execute_counter_1 = bench_exe_counter_gas "1" ~gas_value:2_001

let test_execute_counter_2 = bench_exe_counter_gas "2" ~gas_value:3_001

let test_execute_counter_3 = bench_exe_counter_gas "3" ~gas_value:4_001

let test_execute_counter_4 = bench_exe_counter_gas "4" ~gas_value:5_001

let test_execute_counter_5 = bench_exe_counter_gas "5" ~gas_value:6_001

let test_execute_counter_6 = bench_exe_counter_gas "6" ~gas_value:7_001

let test_execute_counter_7 = bench_exe_counter_gas "7" ~gas_value:8_001

let test_execute_counter_8 = bench_exe_counter_gas "8" ~gas_value:9_001

let test_execute_counter_9 = bench_exe_counter_gas "9" ~gas_value:1001

let tests =
  [
    test_compile_value_0;
    test_compile_value_4096;
    test_compile_ast;
    test_compile_counter;
    test_execute_counter_4096;
    test_execute_counter_0;
    test_execute_counter_1;
    test_execute_counter_2;
    test_execute_counter_3;
    test_execute_counter_4;
    test_execute_counter_5;
    test_execute_counter_6;
    test_execute_counter_7;
    test_execute_counter_8;
    test_execute_counter_9;
  ]

let command = Bench.make_command tests