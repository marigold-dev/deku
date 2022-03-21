open Core_bench
open Vm_utils
let script_incr = [%lambda_vm.script fun x -> (x + 1L, (0L, 0L))]
let script_decr = [%lambda_vm.script fun x -> (x - 1L, (0L, 0L))]

(*pair *)
let script_pair =
  [%lambda_vm.script fun pair -> (fst pair + snd pair, (0L, 0L))]

(* if *)
let script_if =
  [%lambda_vm.script
    fun param ->
      ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]

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
  bench_compile_value_n "incr_lambda" ~initial_gas:101 100L

let test_compile_value_decr_lam =
  bench_compile_value_n "decr_lambda" ~initial_gas:101 32L

(*TODO execute pair first and then descr or incr *)

(* lambda application *)
let lambda_application = [%lambda_vm.script fun y -> (fun x -> (x, (0L, 0L))) y]
let test_compile_value_lam_app =
  bench_compile_value_n "lam_app" ~initial_gas:101 45L

(*let test_execute_lam_app =
  test_execute_n 45L "lambda_app" ~gas_value:101 ~gas_compile:20_000
    ~gas_exe:20_000 ~script:script_lambda*)
let tests =
  [
    test_compile_value_incr_lam;
    test_compile_value_decr_lam;
    test_compile_value_lam_app;
  ]
let command = Bench.make_command tests