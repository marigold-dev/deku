(* Lexer specification for Michelson, to be processed by [ocamllex].

   The underlying design principles are:

     (1) enforce stylistic constraints at a lexical level, in order to
         early reject potentially misleading or poorly written
         Michelson contracts;

     (2) provide precise error messages with hint as how to fix the
         issue, which is achieved by consulting the lexical
         right-context of lexemes;

     (3) be as independent as possible from the Michelson version, so
         upgrades have as little impact as possible on this
         specification: this is achieved by using the most general
         regular expressions to match the lexing buffer and broadly
         distinguish the syntactic categories, and then delegating a
         finer, protocol-dependent, second analysis to an external
         module making the tokens (hence a functor below);

     (4) support unit testing (lexing of the whole input with debug
         traces);

     (5) check the validity of the extensible macros (e.g., PAIR),
         without expansing them (since this requires syntactical
         context in general).

   The limitation to the protocol independence lies in the errors that
   the external module building the tokens (which is
   protocol-dependent) may have to report. Indeed these errors have to
   be contextualised by the lexer in terms of input source regions, so
   useful error messages can be printed, therefore they are part of
   the signature [TOKEN] that parameterise the functor generated
   here. For instance, if, in a future release of Michelson, new
   tokens may be added, and the recognition of their lexemes may
   entail new errors, the signature [TOKEN] will have to be augmented
   and the lexer specification changed. However, it is more likely
   that instructions or types are added, instead of new kinds of
   tokens. *)

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. This enables to externalise
   version-dependent constraints in any module whose signature matches
   [TOKEN]. Generic functions to construct tokens are required.

   Note the predicate [is_eof], which caracterises the virtual token
   for end-of-file, because it requires special handling. Some of
   those functions may yield errors, which are defined as values of
   the types [int_err], [annot_err], and [ident_err]. These errors can
   be better understood by reading the ocamllex specification for the
   lexer ([Lexer.mll]) and the signature of the module [Pair]. *)

type lexeme = string

module type TOKEN =
  sig
    type token
    type t = token

    (* Errors *)

    type int_err = Non_canonical_zero

    type annot_err = Annotation_length of int

    type ident_err =
      Valid_prefix       of Pair.index * Pair.tree
    | Invalid_tree       of Pair.index * char * Pair.tree
    | Truncated_encoding of Pair.index * Pair.child * Pair.tree
    | Missing_break      of int
    | Invalid_identifier

    (* Injections *)

    val mk_string : lexeme -> Region.t -> token
    val mk_bytes  : lexeme -> Region.t -> token
    val mk_int    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
    val mk_annot  : lexeme -> Region.t -> (token, annot_err) result
    val mk_sym    : lexeme -> Region.t -> token
    val eof       : Region.t -> token

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
    val to_region : token -> Region.t

    (* Predicates *)

    val is_string : token -> bool
    val is_bytes  : token -> bool
    val is_int    : token -> bool
    val is_ident  : token -> bool
    val is_annot  : token -> bool
    val is_sym    : token -> bool
    val is_eof    : token -> bool

    val support_string_delimiter : char -> bool
  end

(* The signature of the lexer *)

module type S = LexerLib.API.LEXER

(* The functorised interface *)

module Make (Token : TOKEN) : S
