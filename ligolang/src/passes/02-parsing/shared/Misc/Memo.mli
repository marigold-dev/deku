(* This module defines compilation memos. *)

(* Locations *)

module Region = Simple_utils.Region

type macro = private <
  origin  : Region.t; (* Not ghost   *)
  current : Region.t  (* Maybe ghost *)
>

type location = private
  Loc  of Region.t (* Not ghost *)
| Link of macro

type invalid_loc = Ghost_region

val make_loc :
  Region.t -> (location, invalid_loc) Stdlib.result

val make_link :
  origin:Region.t ->
  current:Region.t ->
  (location, invalid_loc) Stdlib.result

type 'a located = <
  value    : 'a;
  location : location
>

val make_located : value:'a -> location:location -> 'a located

(* Lexemes *)

type lexeme = string location (* Not ghost, empty => EOF *)

type window = <
  valid_lexeme   : lexeme option;
  invalid_lexeme : lexeme
>

val make_window : ?valid:lexeme -> invalid:lexeme -> window

(* Compilation phases *)

type phase =
  Lexer
| Parser  of window
| Scoping

(* Messages *)

type message = private string

type invalid_message = Empty_message

val make_message : string -> (message, invalid_error) Stdlib.result
val string_of_message : message -> string

(* Errors *)

type error = <
  location : location;
  message  : message;  (* Non-empty string (ending with a period)      *)
  hint     : string;   (* Suggestion to solve the issue (may be empty) *)
  help     : string    (* Off-program help (may be empty)              *)
>

val make_error :
  location:location ->
  message:message ->
  hint:string ->
  help:string ->
  error

(* Warnings *)

type warning = <
  location : location;
  message  : message;  (* Non-empty string (ending with a period) *)
  hint     : string;   (* May empty *)
>

val make_warning :
  location:location ->
  message:message ->
  hint:string ->
  warning

(* Kinds of entries *)

type kind =
  Error    of error   (* Failure of an external invariant             *)
| Internal of message (* Failure of an internal invariant (non-empty) *)
| External of message (* Failure of an external process (non-empty)   *)
| Warning  of warning
| Info     of (unit -> message)  (* Log (not-empty) *)

type entry = private <
  phase : phase;
  kind  : kind
>

val make_entry : phase:phase -> kind:kind -> entry

val string_of_entry : file:bool -> entry -> string

(* Memos *)

type memo = <
  mode    : [`Byte | `Point]; (* Bytes vs UTF-8 *)
  offsets : bool;             (* [true] for horizontal offsets *)
  log     : entry FQueue.t;
  enqueue : entry -> memo;
  dequeue : (memo * entry) option
>

type t = memo

val empty_memo : mode:[`Byte | `Point] -> offsets:bool -> memo
