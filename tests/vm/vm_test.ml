open Lambda_vm

type error =
  | Compilation_error of Compiler.error
  | Execution_error   of Interpreter.error

let failwith s = Format.kasprintf failwith s

let wrap ~msg ~computation ~error_pp =
  match computation with
  | Ok value -> value
  | Error error -> failwith msg error_pp error
let compile_exn gas script =
  wrap ~msg:"Compilation_error(%a)"
    ~computation:(Compiler.compile gas script)
    ~error_pp:Compiler.pp_error

let compile_value_exn gas value =
  wrap ~msg:"Compilation_error(%a)"
    ~computation:(Compiler.compile_value gas value)
    ~error_pp:Compiler.pp_error

let execute_exn sender gas arg script =
  let sender = Core.Address.to_string sender in
  let sender = sender in
  let context = Context.make ~sender ~source:sender gas in
  match Interpreter.execute ~context ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" Interpreter.pp_error error

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
    let sender = Core.Address.to_string sender in
    let sender = sender in
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
