(* Printing the CST *)

(* The type [state] captures the state that is threaded in the
    printing iterators in this module. *)

type state

val mk_state :
  offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

(* Printing tokens from the CST in a buffer *)

val print_tokens      : state -> CST.t -> unit
val print_path        : state -> CST.path -> unit
val print_pattern     : state -> CST.pattern -> unit
val print_instruction : state -> CST.instruction -> unit
val print_expr        : state -> CST.expr -> unit
val print_statements  : state -> CST.statements -> unit

(* Printing tokens from the CST in a string *)

val tokens_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.t -> string
val path_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.path -> string
val pattern_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.pattern -> string
val instruction_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.instruction -> string
val type_expr_to_string :
  offsets:bool -> mode:[`Point|`Byte] -> CST.type_expr -> string

(* Pretty-printing of CST nodes *)

val pp_cst  : state -> CST.t -> unit
val pp_expr : state -> CST.expr -> unit
