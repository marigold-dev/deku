module Ast = Ast
module Gas = Gas

module Runtime_limits_error = struct
  type t =
    | Out_of_gas
    | Out_of_stack
  [@@deriving show]
end

let wrap compute =
  match compute () with
  | Ok data -> `Ok data
  | Error error -> `Error error
  | exception Checks.Out_of_stack -> `Limits Runtime_limits_error.Out_of_stack
  | exception Gas.Out_of_gas -> `Limits Runtime_limits_error.Out_of_gas

module Ir = struct
  type script = Ir.script
  type value = Ir.value

  let pp_value = Ir.pp_value
end

module Compiler = struct
  include Compiler

  type compiler_error = Compiler.error =
    (* user program bugs *)
    | Undefined_variable
  [@@deriving show]

  type error =
    | Compiler_error       of compiler_error
    | Runtime_limits_error of Runtime_limits_error.t
  [@@deriving show]

  let wrap compute =
    match wrap compute with
    | `Ok data -> Ok data
    | `Error error -> Error (Compiler_error error)
    | `Limits error -> Error (Runtime_limits_error error)

  let compile gas value = wrap (fun () -> compile gas value)

  let compile_value gas value = wrap (fun () -> compile_value gas value)
end

module Interpreter = struct
  include Interpreter

  type interpreter_error = Interpreter.error =
    (* interpreter bugs *)
    | Undefined_variable
    | Over_applied_primitives
    (* user program bugs *)
    | Value_is_not_pair
    | Value_is_not_int64
    | Value_is_not_function
    | Value_is_not_zero
  [@@deriving show]

  type error =
    | Interpreter_error    of interpreter_error
    | Runtime_limits_error of Runtime_limits_error.t
  [@@deriving show]

  let wrap compute =
    match wrap compute with
    | `Ok data -> Ok data
    | `Error error -> Error (Interpreter_error error)
    | `Limits error -> Error (Runtime_limits_error error)

  let execute gas ~arg script = wrap (fun () -> execute gas ~arg script)
end
