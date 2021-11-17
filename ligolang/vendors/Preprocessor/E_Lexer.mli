(* Module for lexing boolean expressions of conditional directives *)

(* Regions *)

module Region = Simple_utils.Region

val string_of_token : E_Parser.token -> string

(* Lexing boolean expressions (may raise [Error]) *)

exception Error of string Region.reg

val scan : Lexing.lexbuf -> E_Parser.token
