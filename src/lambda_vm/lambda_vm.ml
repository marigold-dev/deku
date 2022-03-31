module Ast = Ast
module Gas = Gas

include Checks

type script = Ir.script
type value = Ir.value

let pp_value = Ir.pp_value

type compile_error = Compiler.error =
  (* user program bugs *)
  | Undefined_variable
[@@deriving show]

include Compiler

type execution_error = Interpreter.error =
  (* interpreter bugs *)
  | Undefined_variable
  | Over_applied_primitives
  (* user program bugs *)
  | Value_is_not_pair
  | Value_is_not_int64
  | Value_is_not_function
  | Value_is_not_zero
[@@deriving show]

include Interpreter
