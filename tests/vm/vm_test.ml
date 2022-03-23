open Lambda_vm

type error =
  | Compilation_error of compile_error
  | Execution_error   of execution_error

let failwith s = Format.kasprintf failwith s

let compile_exn gas script =
  match compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

let compile_value_exn gas value =
  match compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

let execute_exn gas arg script =
  match execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" pp_execution_error error

let execute_ast_exn gas arg script =
  (* TODO: Use different gas to different stuff *)
  let gas = Gas.make ~initial_gas:gas in
  let script = compile_exn gas script in
  let arg = compile_value_exn gas arg in
  execute_exn gas arg script

let execute_ast gas arg script =
  let gas = Gas.make ~initial_gas:gas in
  match (compile_value gas arg, compile gas script) with
  | Ok arg, Ok ir -> (
    match execute gas ~arg ir with
    | Ok result -> Ok result
    | Error error -> Error (Execution_error error))
  | Error error, _
  | _, Error error ->
    Error (Compilation_error error)

module Testable = struct
  let value = Alcotest.of_pp Lambda_vm.pp_value

  let execution_error = Alcotest.of_pp pp_execution_error

  let compilation_error = Alcotest.of_pp pp_compile_error
end
