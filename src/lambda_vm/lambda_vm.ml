module Ast = Ast
module Gas = Gas
module Ir = Ir
module Math = Math

type script = Ir.script [@@deriving show]
type value = Ir.value [@@deriving eq]

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
