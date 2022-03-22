open Core_bench
open Vm_utils

(* increment script *)
let script_incr = [%lambda_vm.script fun x -> (x + 1L, (0L, 0L))]

let test_compile_value_incr = bench_compile_value "incr" ~initial_gas:101 43L

let test_compile_incr =
  bench_compile_script "incr" ~initial_gas:1901 ~script:script_incr

(* decrement script *)
let script_decr = [%lambda_vm.script fun x -> (x - 1L, (0L, 0L))]

let test_compile_value_decr = bench_compile_value "decr" ~initial_gas:101 41L

let test_compile_decr =
  bench_compile_script "decr" ~initial_gas:1901 ~script:script_decr

(* pair *)
let script_pair =
  [%lambda_vm.script fun pair -> (fst pair + snd pair, (0L, 0L))]

let test_compile_value_pair = bench_compile_value "pair" ~initial_gas:101 51L

let test_compile_pair =
  bench_compile_script "pair" ~initial_gas:2901 ~script:script_pair

(* if *)
let script_if =
  [%lambda_vm.script
    fun param ->
      ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]

let test_compile_value_if_true =
  bench_compile_value "if_true" ~initial_gas:101 52L

let test_compile_value_if_false =
  bench_compile_value "if_false" ~initial_gas:101 32L

let test_compile_if =
  bench_compile_script "if" ~initial_gas:4001 ~script:script_if

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

let test_compile_value_decr_lam =
  bench_compile_value "decr_lambda" ~initial_gas:101 32L

let test_compile_lambda =
  bench_compile_script "lambda" ~initial_gas:6501 ~script:script_lambda

(* lambda application *)
let script_lambda_app = [%lambda_vm.script fun y -> (fun x -> (x, (0L, 0L))) y]
let test_compile_value_lam_app =
  bench_compile_value "lam_app" ~initial_gas:101 45L

let test_compile_lambda_app =
  bench_compile_script "lambda_app" ~initial_gas:2000 ~script:script_lambda_app

let tests =
  [
    test_compile_value_incr;
    test_compile_value_decr;
    test_compile_value_pair;
    test_compile_value_if_true;
    test_compile_value_if_false;
    test_compile_value_incr_lam;
    test_compile_value_decr_lam;
    test_compile_value_lam_app;
    test_compile_incr;
    test_compile_decr;
    test_compile_pair;
    test_compile_if;
    test_compile_lambda;
    test_compile_lambda_app;
  ]
let command = Bench.make_command tests