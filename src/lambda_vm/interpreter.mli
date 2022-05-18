type error =
  (* interpreter bugs *)
  | Undefined_variable
  | Over_applied_primitives
  (* user program bugs *)
  | Value_is_not_pair
  | Value_is_not_int64
  | Value_is_not_function
  | Value_is_not_zero
[@@deriving show]

type script_result = {storage : Ir.value; operations : unit}

val execute :
  context:Context.t -> arg:Ir.value -> Ir.code -> (script_result, error) result
