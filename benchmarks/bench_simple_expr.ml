open Core_bench
open Vm_utils

(* increment script *)
let script_incr = [%lambda_vm.script fun x -> (x + 1L, (0L, 0L))]

let test_compile_value_incr = bench_compile_value "incr" ~initial_gas:101 43L

let test_compile_incr =
  bench_compile_script "incr" ~initial_gas:1901 ~script:script_incr

let test_execute_incr =
  bench_execute_exn 42L "incr" ~gas_value:101 ~gas_compile:1901 ~gas_exe:1901
    ~script:script_incr

(* TODO: do I need to check the result storage ? *)
(* decrement script *)
let script_decr = [%lambda_vm.script fun x -> (x - 1L, (0L, 0L))]

let test_compile_value_decr = bench_compile_value "decr" ~initial_gas:101 41L

let test_compile_decr =
  bench_compile_script "decr" ~initial_gas:1901 ~script:script_decr

let test_execute_decr =
  bench_execute_exn 41L "decr" ~gas_value:101 ~gas_compile:1901 ~gas_exe:1901
    ~script:script_decr

(* pair *)
let script_pair =
  [%lambda_vm.script fun pair -> (fst pair + snd pair, (0L, 0L))]

let test_compile_value_pair = bench_compile_value "pair" ~initial_gas:101 51L

let test_compile_value_pair_pair =
  bench_compile_value_pair "pair" ~initial_gas:2901 (23L, 28L)

let test_compile_pair =
  bench_compile_script "pair" ~initial_gas:2901 ~script:script_pair

let test_execute_pair =
  bench_execute_exn_pair "pair" (23L, 28L) ~gas_value:2901 ~gas_compile:2901
    ~gas_exe:2901 ~script:script_pair

(* if *)
let script_if =
  [%lambda_vm.script
    fun param ->
      ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]

let test_compile_value_if_true =
  bench_compile_value "if_true" ~initial_gas:101 52L

let test_compile_value_if_false =
  bench_compile_value "if_false" ~initial_gas:101 32L

let test_compile_value_pair_if_true =
  bench_compile_value_pair "if_true" ~initial_gas:4001 (1L, 51L)

let test_compile_value_pair_if_false =
  bench_compile_value_pair "if_false" ~initial_gas:4001 (0L, 33L)

let test_compile_if =
  bench_compile_script "if" ~initial_gas:4001 ~script:script_if

let test_execute_if_true =
  bench_execute_exn_pair "if_true" (1L, 51L) ~gas_value:4001 ~gas_compile:4001
    ~gas_exe:4001 ~script:script_if

let test_execute_if_false =
  bench_execute_exn_pair "if_false" (0L, 33L) ~gas_value:4001 ~gas_compile:4001
    ~gas_exe:4001 ~script:script_if

(* lambda *)
let script_lambda =
  let incr_lamdba = [%lambda_vm fun x -> x + 1L] in
  let decr_lambda = [%lambda_vm fun x -> x - 1L] in
  [%lambda_vm.script
    fun param ->
      ( (fun inc -> if inc then [%e incr_lamdba] else [%e decr_lambda])
          (fst param) (snd param),
        (0L, 0L) )]
let test_compile_value_incr_lam =
  bench_compile_value "incr_lambda" ~initial_gas:101 100L

let test_compile_value_pair_incr_lam =
  bench_compile_value_pair "incr_lambda" ~initial_gas:6501 (1L, 99L)

let test_compile_value_decr_lam =
  bench_compile_value "decr_lambda" ~initial_gas:101 32L

let test_compile_value_pair_decr_lam =
  bench_compile_value_pair "decr_lambda" ~initial_gas:6501 (0L, 33L)

let test_compile_lambda =
  bench_compile_script "lambda" ~initial_gas:6501 ~script:script_lambda

let test_execute_pair_incr_lam =
  bench_execute_exn_pair "incr_lambda" (1L, 99L) ~gas_value:6501
    ~gas_compile:6501 ~gas_exe:6501 ~script:script_lambda

let test_execute_pair_decr_lam =
  bench_execute_exn_pair "decr_lambda" (0L, 33L) ~gas_value:6501
    ~gas_compile:6501 ~gas_exe:6501 ~script:script_lambda

(* lambda application *)
let script_lambda_app = [%lambda_vm.script fun y -> (fun x -> (x, (0L, 0L))) y]
let test_compile_value_lam_app =
  bench_compile_value "lam_app" ~initial_gas:101 45L

let test_compile_lambda_app =
  bench_compile_script "lambda_app" ~initial_gas:2000 ~script:script_lambda_app

let test_execute_lambda_app =
  bench_execute_exn 45L "lambda_app" ~gas_value:101 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_lambda_app

let tests =
  [
    test_compile_value_incr;
    test_compile_incr;
    test_execute_incr;
    (* descrement *)
    test_compile_value_decr;
    test_compile_decr;
    test_execute_decr;
    (* pair *)
    test_compile_value_pair;
    test_compile_value_pair_pair;
    test_compile_pair;
    test_execute_pair;
    (* if *)
    test_compile_value_if_true;
    test_compile_value_pair_if_true;
    test_compile_value_if_false;
    test_compile_value_pair_if_false;
    test_compile_if;
    test_execute_if_true;
    test_execute_if_false;
    (* lambda *)
    test_compile_value_incr_lam;
    test_compile_value_pair_incr_lam;
    test_compile_value_decr_lam;
    test_compile_value_pair_decr_lam;
    test_compile_lambda;
    test_execute_pair_incr_lam;
    test_execute_pair_decr_lam;
    (* lambda application *)
    test_compile_value_lam_app;
    test_compile_lambda_app;
    test_execute_lambda_app;
  ]
let command = Bench.make_command tests