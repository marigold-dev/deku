open Lambda_vm

type error =
  | Compilation_error of Compiler.error
  | Execution_error   of Interpreter.error

exception Vm_test_error of error

let compile_exn gas script =
  match Compiler.compile gas script with
  | Ok ir -> ir
  | Error error -> raise (Vm_test_error (Compilation_error error))

let compile_value_exn gas value =
  match Compiler.compile_value gas value with
  | Ok value -> value
  | Error error -> raise (Vm_test_error (Compilation_error error))

let execute_exn sender gas arg script =
  let sender = Core_deku.Address.to_string sender in
  let context = Context.make ~sender ~source:sender gas in
  match Interpreter.execute ~context ~arg script with
  | Ok value -> value
  | Error error -> raise (Vm_test_error (Execution_error error))

let execute_ast_exn sender gas arg script =
  (* TODO: Use different gas to different stuff *)
  let gas = Gas.make ~initial_gas:gas in
  let script = compile_exn gas script in
  let arg = compile_value_exn gas arg in
  execute_exn sender gas arg script

let execute_ast sender gas arg script =
  let gas = Gas.make ~initial_gas:gas in
  match (Compiler.compile_value gas arg, Compiler.compile gas script) with
  | Ok arg, Ok ir -> (
    let sender = Core_deku.Address.to_string sender in
    let context = Context.make ~sender ~source:sender gas in
    match Interpreter.execute ~context ~arg ir with
    | Ok result -> Ok result
    | Error error -> Error (Execution_error error))
  | Error error, _
  | _, Error error ->
    Error (Compilation_error error)

module Testable = struct
  let value = Alcotest.of_pp Lambda_vm.Ir.pp_value

  let execution_error = Alcotest.of_pp Interpreter.pp_error

  let compilation_error = Alcotest.of_pp Compiler.pp_error

  let runtime_limits_error = Alcotest.of_pp Runtime_limits_error.pp
end
