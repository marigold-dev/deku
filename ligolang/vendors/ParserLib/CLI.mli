(* Parsing the command-line options for the parser *)

(* Comments *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

(* Preprocessor CLI *)

module type PREPROCESSING_CLI =
  sig
    include COMMENTS

    val input     : string option (* input file     *)
    val extension : string option (* file extension *)
    val dirs      : string list   (* -I             *)
    val show_pp   : bool          (* --show-pp      *)
    val offsets   : bool          (* neg --columns  *)

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    val status : status
  end

(* Lexer CLI *)

module type LEXER_CLI =
  sig
    module Preprocessor_CLI : PREPROCESSING_CLI

    (* Run the preprocessor before lexing *)

    val preprocess : bool

    (* If the value [mode] is [`Byte], then the unit in which source
       positions and regions are expressed in messages is the byte. If
       [`Point], the unit is unicode points. *)

    val mode : [`Byte | `Point]

    (* The value [command] denotes some possible behaviours of the
       compiler. The constructors are

        * [`Copy]: the lexemes of tokens and markup will be printed to
          standard output, with the expectation of a perfect match
          with the input file;

        * [`Units]: the tokens and markup will be printed to standard
          output, that is, the abstract representation of the concrete
          lexical syntax;

        * [`Tokens]: the tokens only will be printed. *)

    val command : [`Copy | `Units | `Tokens] option

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string
    ]

    val status : status
  end

(* Parser CLI *)

module type S =
  sig
    module Lexer_CLI : LEXER_CLI

    (* Use the monolithic API of Menhir. Default is incremental API. *)

    val mono : bool  (* --mono *)

    (* Pretty-print the input. *)

    val pretty : bool (* --pretty *)

    (* Print the AST in ASCII-art. *)

    val cst : bool  (* --cst *)

    (* Reconstruct the tokens from the CST and print them .*)

    val cst_tokens : bool  (* --cst-tokens *)

    (* Enable error recovery *)

    val recovery : bool (* --recovery *)

    (* Enable tracing of error recovery (debug option) *)

    val trace_recovery : bool (* --trace_recovery *)

    (* File path where tracing will be printed ([None] means STDOUT) *)

    val trace_recovery_output : string option

    (* Status *)

    type status = [
      Lexer_CLI.status
    | `DependsOnOtherOption of string * string
    ]

    val status : status
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (Lexer_CLI : LEXER_CLI) : S
