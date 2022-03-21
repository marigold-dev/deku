open Lambda_vm
open Core_bench

(*TODO:change the name to vm_bench_utils *)
let failwith s = Format.kasprintf failwith s

(* compile value *)
let compile_value_exn gas value =
  match compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

let compile_value_n n ~initial_gas =
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

(*TODO: change the name to use without n *)
let bench_compile_value_n s ~initial_gas n =
  let s = s ^ " " ^ Int64.to_string n in
  let name = "compile value " ^ s in
  Bench.Test.create ~name (fun () ->
      let _ = compile_value_n n ~initial_gas in
      ())

(* compile script *)
let compile_exn gas script =
  match compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

let compile_script ~initial_gas script =
  let gas = Gas.make ~initial_gas in
  compile_exn gas script

(* execute script *)
let execute_exn gas arg script =
  match execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" pp_execution_error error

(* change the name to use without the n *)
let bench_execute_n n s ~gas_value ~gas_compile ~gas_exe ~script =
  let name = "execute " ^ s in
  Bench.Test.create ~name (fun () ->
      (* TODO: same code at the one in bench_simple_expr *)
      let arg = compile_value_n n ~initial_gas:gas_value in
      let ir = compile_script ~initial_gas:gas_compile script in
      let gas = Gas.make ~initial_gas:gas_exe in
      let _ = execute_exn gas arg ir in
      ())

let counter_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]
