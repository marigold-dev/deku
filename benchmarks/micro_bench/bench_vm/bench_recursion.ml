open Lambda_vm
open Core_bench
open Bench_vm_utils
open Lambda_vm_tests
open Recursion_tests

(* factorial *)

(* Bench.Test.create_indexed creates a group of
   benchmarks indexed by a size. Which can be helpful in understanding
   non-linearities in the execution profiles of functions
*)

let compile_value_f n f ~initial_gas =
  let n = f (Int64.of_int n) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

(* TODO Note:
   loop from 0 to 1O times,
   - is it necessary for benchmark this function?
*)

let test_compile_value_factorial =
  Bench.Test.create_indexed ~name:"compile value factorial" ~args:[1; 2; 3]
    (fun n ->
      Core.Staged.stage (fun () ->
          for _ = 0 to 10 do
            let _ = compile_value_f n fac ~initial_gas:101 in
            ()
          done))

let test_compile_factorial =
  bench_compile_script "factorial" ~initial_gas:10_000 ~script:factorial_script

let test_execute_factorial =
  Bench.Test.create_indexed ~name:"execute factorial" ~args:[1; 2; 3] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_f n fac ~initial_gas:101 in
          let gas_compile = Gas.make ~initial_gas:10_000 in
          let ir = compile_exn gas_compile factorial_script in
          let gas = Gas.make ~initial_gas:1_000_000 in
          let _ = execute_exn gas arg ir in
          ()))

(* fibonacci *)
let test_compile_value_fib =
  Bench.Test.create_indexed ~name:"compile value fibonacci" ~args:[0; 1; 2]
    (fun n ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_f n fib ~initial_gas:101 in
          ()))

let test_compile_fibonacci =
  bench_compile_script "fibonacci" ~initial_gas:10_000 ~script:fibonacci_script

let test_execute_fibonacci =
  Bench.Test.create_indexed ~name:"execute fibonacci" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_f n fib ~initial_gas:101 in
          let gas_compile = Gas.make ~initial_gas:10_000 in
          let ir = compile_exn gas_compile fibonacci_script in
          let gas = Gas.make ~initial_gas:1_000_000 in
          let _ = execute_exn gas arg ir in
          ()))

(* counter *)
let compile_value_counter n ~initial_gas =
  let n = Int64.of_int n in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let test_compile_value_counter =
  Bench.Test.create_indexed ~name:"compile value counter" ~args:[0; 1; 2]
    (fun n ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_counter n ~initial_gas:101 in
          ()))

let test_execute_counter =
  Bench.Test.create_indexed ~name:"execute counter" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_counter n ~initial_gas:101 in
          let gas_compile = Gas.make ~initial_gas:10_000 in
          let ir = compile_exn gas_compile counter_script in
          let gas = Gas.make ~initial_gas:1_000_000 in
          let _ = execute_exn gas arg ir in
          ()))

let tests =
  [
    test_compile_value_factorial;
    test_compile_value_fib;
    test_compile_value_counter;
    (* compile *)
    test_compile_factorial;
    test_compile_fibonacci;
    (* execute *)
    test_execute_factorial;
    test_execute_fibonacci;
    test_execute_counter;
  ]

let command = Bench.make_command tests
