type error =
  (* user program bugs *)
  [ `Undefined_variable
  | Gas.error
  | Checks.error ]
[@@deriving show]

(* TODO: compile or translate? *)
val compile : Gas.t -> Ast.script -> (Ir.script, error) result
val compile_value : Gas.t -> Ast.value -> (Ir.value, error) result
