(* Checking style based on the lexical context *)

(* Vendor depedencies *)

module Region = Simple_utils.Region
module Core   = LexerLib.Core

(* Style checking function (filter-out) *)

type lex_units = Token.t Core.lex_unit list

type message = string Region.reg

val check : (lex_units, message) result -> (lex_units, message) result
