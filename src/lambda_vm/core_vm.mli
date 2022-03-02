module Ast = Ast
module Gas : module type of Gas

(* ir *)
type script
type value

(* compiler *)
type compile_error = (* user program bugs *)
  | Undefined_variable

val compile : Gas.t -> Ast.script -> (script, compile_error) result
val compile_value : Gas.t -> Ast.value -> (value, compile_error) result

(* interpreter *)
type execution_error =
  (* interpreter bugs *)
  | Undefined_variable
  | Over_applied_primitives
  (* user program bugs *)
  | Value_is_not_pair
  | Value_is_not_int64
  | Value_is_not_function
  | Value_is_not_zero

type script_result = {
  storage : value;
  operations : unit;
}
val execute :
  Gas.t -> arg:value -> script -> (script_result, execution_error) result
