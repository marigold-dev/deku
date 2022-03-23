open Lambda_vm
open Core_bench

let failwith s = Format.kasprintf failwith s

(* compile value *)
let compile_value_exn gas value =
  match compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

(* compile script *)
let compile_exn gas script =
  match compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

(* execute script *)
let execute_exn gas arg script =
  match execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" pp_execution_error error

(* TODO add exn in the function name *)
let bench_compile_value s ~initial_gas n =
  let s = s ^ " " ^ Int64.to_string n in
  let name = "compile value " ^ s in
  Bench.Test.create ~name (fun () ->
      let gas = Gas.make ~initial_gas in
      let _ = compile_value_exn gas (Int64 n) in
      ())

(* TODO add exn *)
let bench_compile_script s ~initial_gas ~script =
  let name = "compile script " ^ s in
  Bench.Test.create ~name (fun () ->
      let gas = Gas.make ~initial_gas in
      let _ = compile_exn gas script in
      ())

(* TODO: check about the init gas *)
let bench_execute_exn n s ~gas_value ~gas_compile ~gas_exe ~script =
  let name = "execute " ^ s in
  Bench.Test.create ~name (fun () ->
      let gas_value = Gas.make ~initial_gas:gas_value in
      let arg = compile_value_exn gas_value (Int64 n) in
      let gas_ir = Gas.make ~initial_gas:gas_compile in
      let ir = compile_exn gas_ir script in
      let gas = Gas.make ~initial_gas:gas_exe in
      let _ = execute_exn gas arg ir in
      ())

(* Pair *)
let compile_value_exn_pair (n1, n2) ~initial_gas =
  let gas_value = Gas.make ~initial_gas in
  compile_value_exn gas_value (Pair (Int64 n1, Int64 n2))

let bench_compile_value_pair s (n1, n2) ~initial_gas =
  let name =
    "compile value pair "
    ^ s
    ^ " ("
    ^ Int64.to_string n1
    ^ ", "
    ^ Int64.to_string n2
    ^ ") " in
  Bench.Test.create ~name (fun () ->
      let _ = compile_value_exn_pair (n1, n2) ~initial_gas in
      ())

(* Note: at [~args] one can add more pairs.
   ex: ~args:[(s_args, (n1, n2); ("second_pair", (n3, n4)))]
*)
let bench_compile_value_pair_parameterised s (n1, n2) ~initial_gas =
  let s_args = " (" ^ Int64.to_string n1 ^ ", " ^ Int64.to_string n2 ^ ") " in
  let name = "compile value " ^ s in
  Bench.Test.create_parameterised ~name
    ~args:[(s_args, (n1, n2))]
    (fun (n1, n2) ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_exn_pair (n1, n2) ~initial_gas in
          ()))

let bench_execute_exn_pair s (n1, n2) ~gas_value ~gas_compile ~gas_exe ~script =
  let name =
    "execute pair "
    ^ s
    ^ " ("
    ^ Int64.to_string n1
    ^ ", "
    ^ Int64.to_string n2
    ^ ") " in
  Bench.Test.create ~name (fun () ->
      let arg = compile_value_exn_pair (n1, n2) ~initial_gas:gas_value in
      let gas_ir = Gas.make ~initial_gas:gas_compile in
      let ir = compile_exn gas_ir script in
      let gas = Gas.make ~initial_gas:gas_exe in
      let _ = execute_exn gas arg ir in
      ())

(* Note: at [~args] one can add more pairs.
   ex: ~args:[(s_args, (n1, n2); ("second_pair", (n3, n4)))]
   - One can add a "for" loop inside the main function
*)
let bench_execute_parameterised s (n1, n2) ~gas_value ~gas_compile ~gas_exe
    ~script =
  let s_args = " (" ^ Int64.to_string n1 ^ ", " ^ Int64.to_string n2 ^ ") " in
  let name = "execute " ^ s in
  Bench.Test.create_parameterised ~name
    ~args:[(s_args, (n1, n2))]
    (fun (n1, n2) ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_exn_pair (n1, n2) ~initial_gas:gas_value in
          let gas_compile = Gas.make ~initial_gas:gas_compile in
          let ir = compile_exn gas_compile script in
          let gas = Gas.make ~initial_gas:gas_exe in
          let _ = execute_exn gas arg ir in
          ()))

let counter_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]
