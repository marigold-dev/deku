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

let test_execute_counter_1 = bench_exe_counter_gas "1" ~gas_value:201

let test_execute_counter_2 = bench_exe_counter_gas "2" ~gas_value:301

let test_execute_counter_3 = bench_exe_counter_gas "3" ~gas_value:401

let test_execute_counter_4 = bench_exe_counter_gas "4" ~gas_value:501

let test_execute_counter_5 = bench_exe_counter_gas "5" ~gas_value:601

let test_execute_counter_6 = bench_exe_counter_gas "6" ~gas_value:701

let test_execute_counter_7 = bench_exe_counter_gas "7" ~gas_value:801

let test_execute_counter_8 = bench_exe_counter_gas "8" ~gas_value:901

let test_execute_counter_9 = bench_exe_counter_gas "9" ~gas_value:1_001

let test_execute_counter_10 = bench_exe_counter_gas "10" ~gas_value:2_001

let test_execute_counter_11 = bench_exe_counter_gas "11" ~gas_value:3_001

let test_execute_counter_12 = bench_exe_counter_gas "12" ~gas_value:4_001

let test_execute_counter_13 = bench_exe_counter_gas "13" ~gas_value:5_001

let test_execute_counter_14 = bench_exe_counter_gas "14" ~gas_value:6_001

let test_execute_counter_15 = bench_exe_counter_gas "15" ~gas_value:7_001

let test_execute_counter_16 = bench_exe_counter_gas "16" ~gas_value:8_001

let test_execute_counter_17 = bench_exe_counter_gas "17" ~gas_value:9_001

let test_execute_counter_18 = bench_exe_counter_gas "18" ~gas_value:10_001

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
    test_execute_counter_10;
    test_execute_counter_11;
    test_execute_counter_12;
    test_execute_counter_13;
    test_execute_counter_14;
    test_execute_counter_15;
    test_execute_counter_16;
    test_execute_counter_17;
    test_execute_counter_18;
  ]

let command = Bench.make_command tests
