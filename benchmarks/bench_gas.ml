open Core_bench
open Lambda_vm
open Vm_utils

(* compile value *)

let compile_value_n n ~initial_gas =
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let test_compile_value_n ~initial_gas n =
  let s = Int64.to_string n in
  let name = "compile value " ^ s in
  Bench.Test.create ~name (fun () ->
      let _ = compile_value_n n ~initial_gas in
      ())
let test_compile_value_0 = test_compile_value_n ~initial_gas:101 0L
let test_compile_value_4096 = test_compile_value_n ~initial_gas:101 4096L

(* compile scripts *)
let counter_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]
let ast_script = [%lambda_vm.script fun x -> x + 1L]

let compile_script ~initial_gas script =
  let gas = Gas.make ~initial_gas in
  compile_exn gas script

let test_compile_script s ~initial_gas script =
  let name = "compile script " ^ s in
  Bench.Test.create ~name (fun () ->
      let _ = compile_script ~initial_gas script in
      ())
let test_compile_ast = test_compile_script "ast" ~initial_gas:501 ast_script
let test_compile_counter =
  test_compile_script "counter" ~initial_gas:14_747_900 counter_script

(* execute scripts *)
let test_execute_n n s ~gas_value ~gas_compile ~gas_exe ~script =
  let name = "execute " ^ s in
  Bench.Test.create ~name (fun () ->
      let arg = compile_value_n n ~initial_gas:gas_value in
      let ir = compile_script ~initial_gas:gas_compile script in
      let gas = Gas.make ~initial_gas:gas_exe in
      let _ = execute_exn gas arg ir in
      ())
let test_execute_counter_0 =
  test_execute_n 0L "counter_0" ~gas_value:101 ~gas_compile:14_747_900
    ~gas_exe:15_000_000 ~script:counter_script

let test_execute_counter_4096 =
  test_execute_n 4096L "counter_4096" ~gas_value:501 ~gas_compile:14_747_900
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