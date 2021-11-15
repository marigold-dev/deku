(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module FQueue = Simple_utils.FQueue

(* Utility types *)

type file_path = string
type lexeme = string

(* The function [rollback] resets the lexing buffer to the state it
   was when it matched the last regular expression. This function is
   safe to use only in the semantic action of the rule which last
   matched. *)

val rollback : Lexing.lexbuf -> unit

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. This function is useful when lexing a file that has been
   previously preprocessed, in which case the argument [file] is the
   name of the file that was preprocessed, _not_ the preprocessed file
   (of which the user is not normally aware). *)

val reset :
  ?file:file_path ->
  ?line:int ->
  ?offset:int ->
  Lexing.lexbuf ->
  unit

(* THREAD FOR STRUCTURED CONSTRUCTS (STRINGS, COMMENTS) *)

(* When scanning structured constructs, like strings and comments, we
   need to keep the region of the opening symbol (like double quote,
   "//" or "(*") in order to report any error more precisely. Since
   ocamllex is byte-oriented, we need to store the parsed bytes as
   characters in an accumulator [acc] and also its length [len], so,
   we are done, it is easy to build the string making up the
   structured construct with [mk_str] (see above).

   The resulting data structure is called a _thread_. (Note for Emacs:
   "*)".)  *)

type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>

val mk_thread : Region.t -> thread

(* STATE *)

(* Beyond producing lexical units (including tokens), the result of
   lexing is a _state_. The type [state] represents the abstract
   logical state of the lexing engine, that is, a value which is
   threaded during scanning and which denotes useful, high-level
   information beyond what the type [Lexing.lexbuf] in the standard
   library already provides for all generic lexers. We qualify it as
   "logical state" because the lexing buffer itself has a "physical
   state" defined by the type [Lexing.lexbuf].

     Tokens are the smallest units used by the parser to build the
   abstract syntax tree. Lexical units include tokens, but also markup
   (whitespace, comments) and preprocessing directives that were
   generated o or not used by the preprocessor, like linemarkers after
   file inclusions. The state includes a queue of recognised tokens,
   with the markup at the left of its lexeme until either the start of
   the file or the end of the previously recognised token.

     The state includes a field [pos] which holds the current position
   in the source file. The position is not always updated after a
   single character has been matched: that depends on the regular
   expression that matched the lexing buffer.

     The field [window] is a two-token window, that is, a buffer that
   contains the last recognised token, and the penultimate (if any).

     The fields [decoder] and [supply] offer the support needed for
   the lexing of UTF-8 encoded characters in comments (the only place
   where they are allowed in our input languages). The former is the
   decoder proper and the latter is the effectful function [supply]
   that takes a byte, a start index and a length and feed it to
   [decoder]. See the documentation of the third-party library Uutf.

     Some methods are now documented.

     The call [state#slide_window token] pushes the token [token] in
   the buffer [lexbuf]. If the buffer is full, that is, it is [Two
   (t1,t2)], then the token [t2] is discarded to make room for
   [token].

     The call [state#sync lexbuf] updates the current position in
   accordance with the contents of the lexing buffer, more precisely,
   depending on the length of the string which has just been
   recognised by the scanner: that length is used as a positive offset
   to the current column. *)

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type command = [`Copy | `Units | `Tokens] option

type 'token config = <
  block     : block_comment option;
  line      : line_comment option;
  input     : file_path option;
  offsets   : bool;
  mode      : [`Byte | `Point];
  command   : command;
  is_eof    : 'token -> bool;
  to_region : 'token -> Region.t;
  to_lexeme : 'token -> string;
  to_string : offsets:bool -> [`Byte | `Point] -> 'token -> string
>

type 'token lex_unit =
  Token     of 'token
| Markup    of Markup.t
| Directive of Directive.t

type 'token state = <
  config       : 'token config;
  window       : 'token window option;
  pos          : Pos.t;
  set_pos      : Pos.t -> 'token state;
  slide_window : 'token -> 'token state;
  sync         : Lexing.lexbuf -> 'token sync;
  decoder      : Uutf.decoder;
  supply       : Bytes.t -> int -> int -> unit;
  mk_line      : thread        -> 'token lex_unit * 'token state;
  mk_block     : thread        -> 'token lex_unit * 'token state;
  mk_newline   : Lexing.lexbuf -> 'token lex_unit * 'token state;
  mk_space     : Lexing.lexbuf -> 'token lex_unit * 'token state;
  mk_tabs      : Lexing.lexbuf -> 'token lex_unit * 'token state;
  mk_bom       : Lexing.lexbuf -> 'token lex_unit * 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}

type message = string Region.reg

(* LEXING COMMENTS AND STRINGS *)

(* Updating the state after scanning a line preprocessing
   directive. *)

val linemarker :
  Region.t ->
  line:string ->
  file:string ->
  ?flag:char ->
  'token state ->
  Lexing.lexbuf ->
  'token lex_unit * 'token state

type 'token scanner =
  'token state ->
  Lexing.lexbuf ->
  ('token lex_unit * 'token state, message) Stdlib.result

type 'token cut =
  thread * 'token state -> 'token lex_unit * 'token state

type 'token client = <
  mk_string                : 'token cut;
  mk_eof                   : 'token scanner;
  callback                 : 'token scanner;
  support_string_delimiter : char -> bool
>

val mk_scan : 'token client -> 'token scanner

(* LEXER INSTANCE *)

(* The function [open_stream] returns a lexer instance made of
     * the input [input] of type [input];
     * a function [read_token] that extracts a token from a lexing
       buffer;
     * a function [read_unit] that extracts a lexical unit (that is, a
       token or whitespace) from a lexing buffer;
     * a lexing buffer [lexbuf] to read tokens from;
     * a function [close] that closes that buffer;
     * a function [window] that returns a window of zero, one or
       two tokens.

     Note that a module [Token] is exported too, because the
   signature of the exported functions depend on it.

     The type [window] is a two-token window, that is, a buffer
   that contains the last recognised token, and the penultimate
   (if any). *)

type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type 'token instance = {
  input      : input;
  read_token : Lexing.lexbuf -> ('token, message) result;
  read_unit  : Lexing.lexbuf -> ('token lex_unit, message) result;
  lexbuf     : Lexing.lexbuf;
  close      : unit -> unit;
  window     : unit -> 'token window option
}

val lexbuf_from_input :
  'token config ->
  input ->
  (Lexing.lexbuf * (unit -> unit), message) Stdlib.result

val open_stream :
  'token config ->
  scan:('token scanner) ->
  input ->
  ('token instance, message) Stdlib.result
