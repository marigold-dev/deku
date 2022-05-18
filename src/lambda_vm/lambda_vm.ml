module Ast = Ast
module Gas = Gas

module Runtime_limits_error = struct
  type t = Out_of_gas | Out_of_stack [@@deriving show]
end

let wrap compute =
  match compute () with
  | Ok data -> `Ok data
  | Error error -> `Error error
  | exception Checks.Out_of_stack -> `Limits Runtime_limits_error.Out_of_stack
  | exception Gas.Out_of_gas -> `Limits Runtime_limits_error.Out_of_gas

module Ir = struct
  include Ir

  module Value_syntax = struct
    let int t = Ir.V_int64 t

    let pair fst snd = Ir.V_pair {first = fst; second = snd}
  end
end

module Compiler = struct
  include Compiler

  type compiler_error = Compiler.error =
    (* user program bugs *)
    | Undefined_variable
  [@@deriving show]

  type error =
    | Compiler_error of compiler_error
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

module Context = Context

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
    | Interpreter_error of interpreter_error
    | Runtime_limits_error of Runtime_limits_error.t
  [@@deriving show]

  let wrap compute =
    match wrap compute with
    | `Ok data -> Ok data
    | `Error error -> Error (Interpreter_error error)
    | `Limits error -> Error (Runtime_limits_error error)

  let execute ~context ~arg script =
    wrap (fun () -> execute ~context ~arg script)
end
