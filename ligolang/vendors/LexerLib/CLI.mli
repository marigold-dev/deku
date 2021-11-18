(* Parsing the command-line options for the lexer *)

(* Comments *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

(* Preprocessor CLI *)

module type PREPROCESSOR_CLI =
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

(* The signature [S] (command-line interface) gathers the options. *)

module type S =
  sig
    module Preprocessor_CLI : PREPROCESSOR_CLI

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

    (* Status *)

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string (* Two conflicting options *)
    ]

    val status : status
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (Preproc_CLI: PREPROCESSOR_CLI) : S
