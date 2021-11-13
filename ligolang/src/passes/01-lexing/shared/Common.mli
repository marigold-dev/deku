(* Internal dependencies *)

module type COMMENTS = Preprocessing_shared.Comments.S

(* Common type definition *)

type lexeme = string

(* Making lexers *)

module Make (Comments : COMMENTS) (Token : Token.S) :
  sig
    module Trace = Simple_utils.Trace
    module Errors = Errors

    type file_path = string

    val from_file    : raise:Errors.t Trace.raise -> file_path -> Token.t list
    val from_string  : raise:Errors.t Trace.raise -> string -> Token.t list
    val from_buffer  : raise:Errors.t Trace.raise -> Buffer.t -> Token.t list
    val from_channel : raise:Errors.t Trace.raise -> in_channel -> Token.t list
    
    (* Aliases *)
    
    val lex_file    : raise:Errors.t Trace.raise -> file_path -> Token.t list
    val lex_string  : raise:Errors.t Trace.raise -> string -> Token.t list
    val lex_buffer  : raise:Errors.t Trace.raise -> Buffer.t -> Token.t list
    val lex_channel : raise:Errors.t Trace.raise -> in_channel -> Token.t list
    
  end
