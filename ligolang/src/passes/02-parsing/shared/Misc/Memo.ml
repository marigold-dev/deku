module Region = Simple_utils.Region

type macro = {
  origin  : Region.t; (* Not ghost   *)
  current : Region.t  (* Maybe ghost *)
}

type location =
  Loc  of Region.t (* Not ghost *)
| Link of macro

(* Regions must not be ghosts and strings must not be empty. *)

type valid_lexeme   = string Region.reg (* Not ghost, not empty.    *)
type invalid_lexeme = string Region.reg (* Not ghost, empty if EOF. *)

type phase =
  Lexer
| Parser  of valid_lexeme option * invalid_lexeme
| Scoping

type error = <
  location : location;
  message  : string;   (* Sentence ending with a period *)
  hint     : string;   (* Suggestion to solve the issue *)
  help     : string    (* Off-program help *)
>

type invalid_error = Ghost_region

let check_loc = function
  Loc reg ->
    if reg#is_ghost then
      Stdlib.Error Ghost_region
    else Ok ()
| Link {origin; _} ->
    if origin#is_ghost then
      Stdlib.Error Ghost_region
    else Ok ()

let make_error ~location ~message ~hint ~help =
  match check_loc location with
    Stdlib.Ok () ->
      Ok (object
            method location = location
            method message  = message
            method hint     = hint
            method help     = help
          end)
  | Error _ as e -> e

type warning = <
  location : location;
  message  : string;   (* Sentence ending with a period *)
  hint     : string;   (* Idem *)
>

type invalid_warning = invalid_error

let make_warning ~location ~message ~hint =
  match check_loc location with
    Stdlib.Ok () ->
      Ok (object
            method location = location
            method message  = message
            method hint     = hint
            method help     = help
          end)
  | Error _ as e -> e

type kind =
  Error    of error   (* Failure of an external invariant *)
| Internal of string  (* Failure of an internal invariant *)
| External of string  (* Failure of an external process   *)
| Warning  of warning
| Info     of (unit -> string)  (* Log *)

type entry = <
  phase : phase;
  kind  : kind
>

type invalid_entry =
  Ghost_lexeme
| Empty_lexeme

let check_phase = function
  Parser (Some valid_lexeme, invalid_lexeme) ->
    let open Region in
    if  valid_lexeme.region#is_ghost
    ||  invalid_lexeme.region#is_ghost
    then Stdlib.Error Ghost_lexeme
    else if valid_lexeme.value = ""
    then Stdlib.Error Empty_lexeme
    else Ok ()
| Parser (None, invalid_lexeme) ->
    if   invalid_lexeme.region#is_ghost
    then Stdlib.Error Ghost_lexeme
    else Ok ()
| Lexer
| Scoping -> Ok ()

let make_entry ~phase ~kind =
  match check_phase phase with
    Stdlib.Error _ as e -> e
  | Ok () -> Ok (object
                  method phase = phase
                  method kind  = kind
                end)

type memo = <
  mode    : [`Byte | `Point]; (* Bytes vs UTF-8 *)
  offsets : bool;             (* [true] for horizontal offsets *)
  log     : entry FQueue.t
>

type t = memo

let empty_memo ~mode ~offsets : memo =
  object
    method mode          = mode
    method offsets       = offsets
    method log           = FQueue.empty
    method enqueue entry = {< log = FQueue.enq entry log >}
    method dequeue       =
      match FQueue.deq log with
                     None -> None
      | Some (log, entry) -> Some ({< log=log >}, entry)
  end

let sprintf = Printf.sprintf

let string_of_entry ~(file:bool) entry : string =
  let reg = entry#region#to_string
              ~file
              ~offsets:entry#offsets
              error#mode in
  let string =
    match error#phase with
      Parser (None, invalid_lexeme) ->
       (match invalid_lexeme.Region.value with
              "" -> sprintf "Parse error %s" reg (* EOF *)
        | lexeme -> sprintf "Parse error %s, before \"%s\""
                           reg lexeme)
    | Parser (Some valid_lexeme, invalid_lexeme) ->
       let string =
         sprintf "Parse error %s, after \"%s\""
                 reg valid_lexeme.Region.value in
       (match invalid_lexeme.Region.value with
              "" -> string (* EOF *)
        | lexeme -> sprintf "%s and before \"%s\"" string lexeme)
    | Lexer ->
        sprintf "Lexical error %s" reg
    | Scoping ->
       sprintf "Scoping error %s" reg in
  let string =
    string
    ^ (if error#message = "" then "."
       else ":\n" ^ error#message) ^ "\n" in
  let string =
    string ^ (if error#hint = "" then ""
              else sprintf "Hint: %s\n" error#hint)
  in string
