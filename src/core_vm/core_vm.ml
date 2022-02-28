module Ast = Ast
module Gas = Gas

include Ir

type compile_error = Compiler.error =
  (* user program bugs *)
  | Undefined_variable

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

include Interpreter
