open Core_bench
open Lambda_vm
open Vm_utils

(* Test Lambda VM *)
let counter_script =
  [%lambda_vm.script fun x ->
    (
      (fun f -> f f x)
      (fun f n ->
        if n then 
          1L + f f (n - 1L)
      else
        0L),
        (0L, 0L) )]

let test_compile_value_0L =
  Bench.Test.create ~name:"compile value 0L" 
  (fun () -> 
    let gas = Gas.make ~initial_gas:101 in
    let _ = compile_value_exn gas (Int64 0L) in 
    ()
  )

let test_compile_value_4096L =
  Bench.Test.create ~name:"value 4096L" 
  (fun () -> 
    let gas = Gas.make ~initial_gas:101 in
    let _ = compile_value_exn gas (Int64 4096L) in 
    ()
  )

let test_compile_counter =
  Bench.Test.create ~name:"counter"
  (fun () ->
    let gas = Gas.make ~initial_gas:14747900 in 
    let _ = compile_exn gas counter_script in 
    ()
    )

let tests = 
  [test_compile_value_0L; 
  test_compile_value_4096L; 
  test_compile_counter ]

let command = Bench.make_command tests