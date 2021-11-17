(* Interfacing the ReasonLIGO lexer. *)

(* Vendors dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Token  = Lexing_reasonligo.Token
module Errors = Lexing_shared.Errors

type file_path = string

(* Lexing various sources *)

val from_file    : raise:Errors.t Trace.raise -> file_path -> Token.t list
val from_string  : raise:Errors.t Trace.raise -> string -> Token.t list
val from_buffer  : raise:Errors.t Trace.raise -> Buffer.t -> Token.t list
val from_channel : raise:Errors.t Trace.raise -> in_channel -> Token.t list

(* Aliases *)

val lex_file    : raise:Errors.t Trace.raise -> file_path -> Token.t list
val lex_string  : raise:Errors.t Trace.raise -> string -> Token.t list
val lex_buffer  : raise:Errors.t Trace.raise -> Buffer.t -> Token.t list
val lex_channel : raise:Errors.t Trace.raise -> in_channel -> Token.t list
