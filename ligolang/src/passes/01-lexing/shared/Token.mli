(* This signature defines the lexical tokens for LIGO

   _Tokens_ are the abstract units which are used by the parser to
   build the abstract syntax tree (AST), in other words, the stream of
   tokens is the minimal model of the input program, carrying
   implicitly all its structure in a linear encoding, and nothing
   else, in particular, comments and whitespace are absent.

     A _lexeme_ is a specific character string (concrete
   representation) denoting a token (abstract representation). Tokens
   can be thought of as sets, and lexemes as elements of those sets --
   there is often an infinite number of lexemes, but a small number of
   tokens. (Think of identifiers as lexemes and one token.)

     The tokens are qualified here as being "lexical" because the
   parser generator Menhir expects to define them, in which context
   they are called "parsing tokens", and they are made to match each
   other. (This is an idiosyncratic terminology.)

     The type of the lexical tokens is the variant [t], also
   aliased to [token].

     The signature [S] exports an abstract type [token], so a lexer
   can be a functor over tokens. This enables to externalise
   version-dependent constraints in any module whose signature matches
   [S]. Generic functions to construct tokens are required.

     Note the predicate [is_eof], which caracterises the virtual token
   for end-of-file, because it requires special handling. Some of
   those functions may yield errors, which are defined as values of
   the type [int_err] etc. These errors can be better understood by
   reading the ocamllex specification for the lexer ([Lexer.mll]). *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* TOKENS *)

type lexeme = string

module type S =
  sig
    type token
    type t = token

    (* Projections

       The difference between extracting the lexeme and a string from
       a token is that the latter is the textual representation of the
       OCaml value denoting the token (its abstract syntax), rather
       than its lexeme (concrete syntax). Note that [concrete] is used
       by the modukle [UnlexerGen] to transform the textual
       representation of a token (not a lexeme) into a lexeme. *)

    val to_lexeme : token -> lexeme
    val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
    val to_region : token -> Region.t
    val concrete  : string -> lexeme

    (* Injections *)

    type   int_err = Non_canonical_zero
    type mutez_err = Unsupported_mutez_syntax
                   | Non_canonical_zero_tez
    type   nat_err = Invalid_natural
                   | Unsupported_nat_syntax
                   | Non_canonical_zero_nat
    type   sym_err = Invalid_symbol of string
    type  lang_err = Unsupported_lang_syntax
    type   kwd_err = Invalid_keyword

    val mk_int      : lexeme -> Region.t -> (token,   int_err) result
    val mk_nat      : lexeme -> Region.t -> (token,   nat_err) result
    val mk_mutez    : lexeme -> Region.t -> (token, mutez_err) result
    val mk_sym      : lexeme -> Region.t -> (token,   sym_err) result
    val mk_kwd      : lexeme -> Region.t -> (token,   kwd_err) result
    val mk_ident    : lexeme -> Region.t -> token
    val mk_string   : lexeme -> Region.t -> token
    val mk_verbatim : lexeme -> Region.t -> token
    val mk_bytes    : lexeme -> Region.t -> token
    val mk_uident   : lexeme -> Region.t -> token
    val mk_attr     : lexeme -> Region.t -> token
    val mk_lang     : lexeme Region.reg -> Region.t -> (token, lang_err) result
    val mk_eof      : Region.t -> token

    (* Predicates *)

    val is_eof      : token -> bool

    val support_string_delimiter : char -> bool
    val verbatim_delimiters : string * string

  end
