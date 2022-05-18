type error = (* user program bugs *)
  | Undefined_variable [@@deriving show]

(* TODO: compile or translate? *)

val compile : Gas.t -> Ast.script -> (Ir.code, error) result

val compile_value : Gas.t -> Ast.value -> (Ir.value, error) result
