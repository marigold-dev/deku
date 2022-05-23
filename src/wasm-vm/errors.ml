type t =
  [ `Initialization_error
  | `Module_validation_error
  | `Execution_error ]
[@@deriving show]
exception Error of t
let raise e = raise (Error e)
