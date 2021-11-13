(* This module exports checks on scoping, called from the parser. *)

module Region = Simple_utils.Region
module CST    = Cst.Jsligo
module Token  = Lexing_jsligo.Token

type window = <
  last_token    : Token.t option;
  current_token : Token.t
>

exception Error of string * window

val check_reserved_name : CST.variable -> unit
(* val check_pattern       : CST.pattern -> unit *)
(* val check_variants      : CST.variant Region.reg list -> unit *)
val check_fields        : CST.field_decl Region.reg list -> unit
