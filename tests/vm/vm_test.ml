open Lambda_vm

type error =
  | Compilation_error of Compiler.error
  | Execution_error   of Interpreter.error

let failwith s = Format.kasprintf failwith s

let compile_exn gas script =
  match Compiler.compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" Compiler.pp_error error

let compile_value_exn gas value =
  match Compiler.compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" Compiler.pp_error error

let execute_exn gas arg script =
  match Interpreter.execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" Interpreter.pp_error error

let execute_ast_exn gas arg script =
  (* TODO: Use different gas to different stuff *)
  let gas = Gas.make ~initial_gas:gas in
  let script = compile_exn gas script in
  let arg = compile_value_exn gas arg in
  execute_exn gas arg script

let execute_ast gas arg script =
  let gas = Gas.make ~initial_gas:gas in
  match (Compiler.compile_value gas arg, Compiler.compile gas script) with
  | Ok arg, Ok ir -> (
    match Interpreter.execute gas ~arg ir with
    | Ok result -> Ok result
    | Error error -> Error (Execution_error error))
  | Error error, _
  | _, Error error ->
    Error (Compilation_error error)

module Testable = struct
  let value = Alcotest.of_pp Ir.pp_value

  let execution_error = Alcotest.of_pp Interpreter.pp_error

  let compilation_error = Alcotest.of_pp Compiler.pp_error
end
