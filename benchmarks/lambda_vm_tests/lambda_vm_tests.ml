open Lambda_vm

let failwith s = Format.kasprintf failwith s

(* compile value *)
let compile_value_exn gas value =
  match Compiler.compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" Compiler.pp_error error

(* compile script *)
let compile_exn gas script =
  match Compiler.compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" Compiler.pp_error error

(* execute script *)
let execute_exn gas arg script =
  match Interpreter.execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" Interpreter.pp_error error

let counter_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]

module Primitives_tests = Primitives_tests
module Recursion_tests = Recursion_tests
module Simple_expr_tests = Simple_expr_tests
