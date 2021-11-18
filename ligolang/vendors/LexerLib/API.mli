(* Using Core to make UTF-8 aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Generic signature of input lexers *)

module type LEXER =
  sig
    type token

    val scan : token Core.scanner
  end

(* The functor itself *)

module type S =
  sig
    (* The traditional API offers functions to lex various inputs and
       return token instances (see type [Core.instance]). *)

    type token

    type file_path = string
    type message   = string Region.reg

    type ('src,'dst) lexer =
      token Core.config ->
      'src ->
      ('dst, message) Stdlib.result

    val from_lexbuf  : (Lexing.lexbuf, token Core.instance) lexer
    val from_channel : (in_channel,    token Core.instance) lexer
    val from_string  : (string,        token Core.instance) lexer
    val from_buffer  : (Buffer.t,      token Core.instance) lexer
    val from_file    : (file_path,     token Core.instance) lexer

    (* The advanced API offers functions to lex all tokens from all
       sources (module [Tokens]) or to lex all lexical units (module
       [Units]), that is, tokens and markup (see module [Markup]. *)

    module Tokens :
      sig
        val from_lexbuf  : (Lexing.lexbuf, token list) lexer
        val from_channel : (in_channel,    token list) lexer
        val from_string  : (string,        token list) lexer
        val from_buffer  : (Buffer.t,      token list) lexer
        val from_file    : (file_path,     token list) lexer
      end

    module LexUnits :
      sig
        type nonrec 'src lexer = ('src, token Core.lex_unit list) lexer

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end
  end

module Make (Lexer : LEXER) : S with type token = Lexer.token
