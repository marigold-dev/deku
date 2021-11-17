(* Printing the CST *)

(* The type [state] captures the state that is threaded in the
    printing iterators in this module. *)

type state

val mk_state :
  offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

(* Printing tokens from the CST in a buffer *)

val print_tokens  : state -> CST.t -> unit

(* Printing tokens from the CST in a string *)

val tokens_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.t -> string
val type_expr_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.type_expr -> string
val expr_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.expr -> string

(* Pretty-printing *)

val pp_cst  : state -> CST.t -> unit
