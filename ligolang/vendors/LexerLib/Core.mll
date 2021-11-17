(* A library for writing UTF8-aware lexers *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module FQueue = Simple_utils.FQueue
module Utils  = Simple_utils.Utils

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf) in
  let pos_cnum = lexbuf.lex_curr_p.pos_cnum - len in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum}

(* LEXER ENGINE *)

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line lexbuf] modifies in-place the lexing
   buffer [lexbuf] so the lexing engine records that the file
   associated with [lexbuf] is named [file], and the current line is
   [line]. *)

let reset_file file lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file}

let reset_line line lexbuf =
  assert (line >= 0);
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = line}

let reset_offset offset lexbuf =
  assert (offset >= 0);
  let open Lexing in
  let bol = lexbuf.lex_curr_p.pos_bol in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum = bol + offset }

let reset ?file ?(line=1) ?offset lexbuf =
  let () =
    match file with
      Some file -> reset_file file lexbuf
    |      None -> () in
  let () = reset_line line lexbuf in
  match offset with
    Some offset -> reset_offset offset lexbuf
  |        None -> ()

(* Utility types *)

type file_path = string
type lexeme = string

(* THREAD FOR STRUCTURED CONSTRUCTS (STRINGS, COMMENTS) *)

type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>

let mk_thread region : thread =
  (* The call [explode s a] is the list made by pushing the characters
     in the string [s] on top of [a], in reverse order. For example,
     [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)

  let explode s acc =
    let rec push = function
      0 -> acc
    | i -> s.[i-1] :: push (i-1)
    in push (String.length s) in
  object
    val opening = region
    method opening = opening

    val length = 0
    method length = length

    val acc = []
    method acc = acc

    method set_opening opening = {< opening; length; acc >}

    method push_char char =
      {< opening; length=length+1; acc=char::acc >}

    method push_string str =
      {< opening;
         length = length + String.length str;
         acc = explode str acc >}

    (* The value of [thread#to_string] is a string of length
       [thread#length] containing the characters in the list
       [thread#acc], in reverse order. For instance, [thread#to_string
       = "abc"] if [thread#length = 3] and [thread#acc =
       ['c';'b';'a']]. *)

    method to_string =
      let bytes = Bytes.make length ' ' in
      let rec fill i = function
        [] -> bytes
      | char::l -> Bytes.set bytes i char; fill (i-1) l
      in fill (length-1) acc |> Bytes.to_string
  end

(* STATE *)

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
  mk_line      :        thread -> 'token lex_unit * 'token state;
  mk_block     :        thread -> 'token lex_unit * 'token state;
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

type 'token scanner =
  'token state ->
  Lexing.lexbuf ->
  ('token lex_unit * 'token state, message) Stdlib.result

type 'token cut =
  thread * 'token state -> 'token lex_unit * 'token state

(* The type [client] gathers the arguments to the lexer in this
    module. *)

type 'token client = <
  mk_string                : 'token cut;
  mk_eof                   : 'token scanner;
  callback                 : 'token scanner;
  support_string_delimiter : char -> bool
>

type 'token internal_scanner =
  'token state -> Lexing.lexbuf -> 'token lex_unit * 'token state

type 'token internal_client = <
  mk_string                : 'token cut;
  mk_eof                   : 'token internal_scanner;
  callback                 : 'token internal_scanner;
  support_string_delimiter : char -> bool
>

let mk_state ~config ~window ~pos ~decoder ~supply : 'token state =
  object (self)
    method config  = config
    val window     = window
    method window  = window
    val pos        = pos
    method pos     = pos
    method decoder = decoder
    method supply  = supply

    method set_pos pos = {< pos = pos >}

    method slide_window new_token =
      let new_window =
        match self#window with
          None ->
            object
              method last_token    = None
              method current_token = new_token
            end
        | Some window ->
            object
              method last_token    = Some window#current_token
              method current_token = new_token
            end
      in {< window = Some new_window >}

    method sync lexbuf : 'token sync =
      let lexeme = Lexing.lexeme lexbuf in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in {region; lexeme; state}

    (* MARKUP *)

    (* Committing markup to the current logical state *)

    method mk_newline lexbuf =
      let ()     = Lexing.new_line lexbuf in
      let value  = Lexing.lexeme lexbuf in
      let start  = self#pos in
      let stop   = start#new_line value in
      let region = Region.make ~start ~stop in
      let markup = Markup.Newline Region.{region; value}
      in Markup markup, self#set_pos stop

    method mk_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.LineCom Region.{region; value}
      in Markup markup, self

    method mk_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.BlockCom Region.{region; value}
      in Markup markup, self

    method mk_space lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Space Region.{region; value}
      in Markup markup, state

    method mk_tabs lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Tabs Region.{region; value}
      in Markup markup, state

    method mk_bom lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = lexeme in
      let markup = Markup.BOM Region.{region; value}
      in Markup markup, state
  end

(* LEXER INSTANCE *)

(* Pretty-printing a lexical unit *)

let output_unit config out_channel lex_unit =
  let output    str = Printf.fprintf out_channel "%s%!" str in
  let output_nl str = output (str ^ "\n")
  and offsets = config#offsets
  and mode    = config#mode in
  match config#command with
    Some `Tokens ->
      (match lex_unit with
         Token token ->
           config#to_string ~offsets mode token |> output_nl
       | Markup _ | Directive _ -> ()) (* Only tokens *)
  | Some `Copy ->
     let lexeme =
       match lex_unit with
         Token token -> config#to_lexeme token
       | Markup m    -> Markup.to_lexeme m
       | Directive d -> Directive.to_lexeme d
     in output lexeme
  | Some `Units ->
      let string =
        match lex_unit with
          Token token -> config#to_string ~offsets mode token
        | Markup m    -> Markup.to_string ~offsets mode m
        | Directive d -> Directive.to_string ~offsets mode d
      in output_nl string
  | None -> ()

(* The lexer instance: the main exported data type *)

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

let lexbuf_from_input config = function
  String s ->
    Ok (Lexing.from_string s, fun () -> ())
| Channel chan ->
    let close () = close_in chan in
    Ok (Lexing.from_channel chan, close)
| Buffer b ->
    let () =
      match config#input with
        None | Some "" -> ()
      | Some path -> reset ~file:path b
    in Ok (b, fun () -> ())
| File "" ->
    Stdlib.Error (Region.wrap_ghost "File not found.")
| File path ->
    try
      let channel  = open_in path in
      let close () = close_in channel in
      let lexbuf   = Lexing.from_channel channel in
      let ()       = reset ~file:path lexbuf
      in Ok (lexbuf, close)
    with Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)

(* Errors (NOT EXPORTED) *)

exception Error of string Region.reg

(* Encoding a function call in exception-raising style (ERS) to
   error-passing style (EPS) *)

let lift scanner lexbuf =
  try Stdlib.Ok (scanner lexbuf) with
    Error msg -> Stdlib.Error msg

(* Decoding a function call in EPS to ERS *)

let drop scanner lexbuf =
  match scanner lexbuf with
    Stdlib.Ok state -> state
  | Error msg -> raise (Error msg)

(* The main function *)

let open_stream config ~scan input =
  let log       = output_unit config stdout
  and scan      = Utils.(drop <@ scan)
  and file_path = match config#input with
                    Some path -> path
                  | _ -> ""
  and   decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let    supply = Uutf.Manual.src decoder in
  let     state = ref (mk_state
                         ~config
                         ~window:None
                         ~pos:(Pos.min ~file:file_path)
                         ~decoder
                         ~supply) in
  let window () = !state#window in

  let read_unit lexbuf =
    let unit, state' = scan !state lexbuf in
    let () = log unit in
    let () = state := state' in
    let () =
      match unit with
        Token token -> state := !state#slide_window token
      | Markup _ | Directive _ -> ()
    in unit in

  let rec read_token lexbuf =
    match read_unit lexbuf with
                 Token token -> token
    | Markup _ | Directive _ -> read_token lexbuf in

  match lexbuf_from_input config input with
    Stdlib.Ok (lexbuf, close) ->
      let read_unit  = lift read_unit
      and read_token = lift read_token in
      Ok {read_unit; read_token; input; lexbuf; close; window}
  | Error _ as e -> e

(* LEXING COMMENTS AND STRINGS *)

(* Errors *)

type error =
  Invalid_utf8_sequence
| Unterminated_comment of string
| Unterminated_string
| Broken_string
| Invalid_character_in_string
| Undefined_escape_sequence
| Invalid_linemarker_argument

let sprintf = Printf.sprintf

let error_to_string = function
  Invalid_utf8_sequence ->
    "Invalid UTF-8 sequence."
| Undefined_escape_sequence ->
    "Undefined escape sequence.\n\
     Hint: Remove or replace the sequence."
| Unterminated_string ->
    "Unterminated string.\n\
     Hint: Close with double quotes."
| Unterminated_comment ending ->
    sprintf "Unterminated comment.\n\
             Hint: Close with %S." ending
| Broken_string ->
    "The string starting here is interrupted by a line break.\n\
     Hint: Remove the break, close the string before or insert a \
     backslash."
| Invalid_character_in_string ->
    "Invalid character in string.\n\
     Hint: Remove or replace the character."
| Invalid_linemarker_argument ->
    "Unexpected or invalid linemarker argument.\n\
     Hint: The optional argument is either 1 or 2."

let fail region error =
  let value = error_to_string error in
  raise (Error Region.{value; region})

(* Reading UTF-8 encoded characters *)

let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
  let ()             = rollback lexbuf in
  let len            = thread#length in
  let thread, status = scan_utf8 thread state lexbuf in
  let delta          = thread#length - len in
  let stop           = state#pos#shift_one_uchar delta in
  match status with
    Ok () -> callback thread (state#set_pos stop) lexbuf
  | Stdlib.Error error ->
      let region = Region.make ~start:state#pos ~stop
      in fail region error

(* An input program may contain preprocessing directives, and the
   entry modules (named *Main.ml) run the preprocessor on them, as if
   using the GNU C preprocessor in traditional mode:

   https://gcc.gnu.org/onlinedocs/cpp/Traditional-Mode.html

     The main interest in using a preprocessor is that it can stand
   for a poor man's (flat) module system thanks to #include
   directives, and the equivalent of the traditional mode leaves the
   markup undisturbed.

     Linemarkers (that is, line directives) may carry some additional
   flags:

   https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

   of which we will retain in our context 1 and 2 (either 1 or 2),
   indicating, respectively, the start of a new file and the return
   from a file (after its inclusion has been processed). *)

let linemarker prefix ~line ~file ?flag state lexbuf =
  let {state; region; _} = state#sync lexbuf in
  let flag      = match flag with
                    Some '1' -> Some Directive.Push
                  | Some '2' -> Some Directive.Pop
                  | _        -> None in
  let linenum   = int_of_string line in
  let value     = linenum, file, flag in
  let region    = Region.cover prefix region in
  let directive = Directive.Linemarker Region.{value; region} in
  let pos       = region#start#add_nl in
  let pos       = (pos#set_file file)#set_line linenum
  in Directive directive, state#set_pos pos

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let utf8_bom   = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let esc        = "\\n" | "\\\"" | "\\\\" | "\\b"
               | "\\r" | "\\t" | "\\x" byte
let flag       = '1' | '2' (* Linemarkers *)

(* Comment delimiters *)

let pascaligo_block_comment_opening  = "(*"
let pascaligo_block_comment_closing  = "*)"
let pascaligo_line_comment           = "//"

let cameligo_block_comment_opening   = "(*"
let cameligo_block_comment_closing   = "*)"
let cameligo_line_comment            = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment          = "//"

let michelson_block_comment_opening = "/*"
let michelson_block_comment_closing = "*/"
let michelson_line_comment          = "#"

let jsligo_block_comment_opening    = "/*"
let jsligo_block_comment_closing    = "*/"
let jsligo_line_comment             = "//"

let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening
| jsligo_block_comment_opening

let block_comment_closings =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing
| michelson_block_comment_closing
| jsligo_block_comment_opening

let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment
| jsligo_line_comment

(* RULES (SCANNERS) *)

rule scan client state = parse
  (* Markup *)

  nl    { state#mk_newline lexbuf }
| ' '+  { state#mk_space   lexbuf }
| '\t'+ { state#mk_tabs    lexbuf }

  (* Strings *)
| '\''
| '"' as lexeme {
  if client#support_string_delimiter lexeme then
    let {region; state; _} = state#sync lexbuf in
    let thread             = mk_thread region in
    scan_string lexeme thread state lexbuf |> client#mk_string
  else (rollback lexbuf; client#callback state lexbuf)
}

  (* Comment *)

| block_comment_openings as lexeme {
    match state#config#block with
      Some block when block#opening = lexeme ->
        let {region; state; _} = state#sync lexbuf in
        let thread             = mk_thread region in
        let thread             = thread#push_string lexeme in
        let thread, state      = scan_block block thread state lexbuf
        in state#mk_block thread
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }

| line_comments as lexeme {
    match state#config#line with
      Some line when line = lexeme ->
        let {region; state; _} = state#sync lexbuf in
        let thread             = mk_thread region in
        let thread             = thread#push_string lexeme in
        let thread, state      = scan_line thread state lexbuf
        in state#mk_line thread
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* {
    let {state; region; _} = state#sync lexbuf
    in eol region line file flag state lexbuf }

  (* Other tokens *)

| eof { client#mk_eof state lexbuf }

| _ { rollback lexbuf;
      client#callback state lexbuf (* May raise exceptions *) }

(* Finishing a linemarker *)

and eol region line file flag state = parse
  nl | eof { linemarker region ~line ~file ?flag state lexbuf }
| _        { let {region; _} = state#sync lexbuf
             in fail region Invalid_linemarker_argument }

(* Block comments

   (For Emacs: ("(*") The lexing of block comments must take care of
   embedded block comments that may occur within, as well as strings,
   so no substring "*/" or "*)" may inadvertently close the
   block. This is the purpose of the first case of the scanner
   [scan_block]. *)

and scan_block block thread state = parse
  block_comment_openings as lexeme {
    if   block#opening = lexeme
    then let opening            = thread#opening in
         let {region; state; _} = state#sync lexbuf in
         let thread             = thread#push_string lexeme in
         let thread             = thread#set_opening region in
         let scan_next          = scan_block block in
         let thread, state      = scan_next thread state lexbuf in
         let thread             = thread#set_opening opening
         in scan_block block thread state lexbuf
    else begin
           rollback lexbuf;
           scan_char_in_block block thread state lexbuf
         end }

| block_comment_closings as lexeme {
    if   block#closing = lexeme
    then thread#push_string lexeme, (state#sync lexbuf).state
    else begin
           rollback lexbuf;
           scan_char_in_block block thread state lexbuf
         end }

| nl as nl {
    let ()     = Lexing.new_line lexbuf
    and state  = state#set_pos (state#pos#new_line nl)
    and thread = thread#push_string nl in
    scan_block block thread state lexbuf }

| eof { let err = Unterminated_comment block#closing
        in fail thread#opening err }

| _ { rollback lexbuf;
      scan_char_in_block block thread state lexbuf }

and scan_char_in_block block thread state = parse
  _ { let if_eof thread =
        let err = Unterminated_comment block#closing
        in fail thread#opening err in
      let scan_utf8 = scan_utf8_char if_eof
      and callback  = scan_block block in
      scan_utf8_wrap scan_utf8 callback thread state lexbuf }

(* Line comments *)

and scan_line thread state = parse
  nl as nl { let ()     = Lexing.new_line lexbuf
             and thread = thread#push_string nl
             and state  = state#set_pos (state#pos#new_line nl)
             in thread, state }
| eof      { thread, state }
| _        { let scan_utf8 = scan_utf8_char (fun _ -> Stdlib.Ok ())
             in scan_utf8_wrap scan_utf8 scan_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8_char if_eof thread state = parse
     eof { thread, if_eof thread }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _
           | `End         -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8_char if_eof thread state lexbuf }

(* Scanning strings *)

and scan_string delimiter thread state = parse
  nl     { fail thread#opening Broken_string }
| eof    { fail thread#opening Unterminated_string }
| ['\t' '\r' '\b']
         { let {region; _} = state#sync lexbuf
           in fail region Invalid_character_in_string }
| '"'    {
  if delimiter = '"' then
    let {state; _} = state#sync lexbuf
        in thread, state
  else
    let {state; _} = state#sync lexbuf in
           scan_string delimiter (thread#push_char '"') state lexbuf
  }
| '\''   {
  if delimiter = '\'' then
    let {state; _} = state#sync lexbuf
        in thread, state
  else
    let {state; _} = state#sync lexbuf in
           scan_string delimiter (thread#push_char '\'') state lexbuf

}
| esc    { let {lexeme; state; _} = state#sync lexbuf in
           let thread = thread#push_string lexeme
           in scan_string delimiter thread state lexbuf }
| '\\' _ { let {region; _} = state#sync lexbuf
           in fail region Undefined_escape_sequence }
| _ as c { let {state; _} = state#sync lexbuf in
           scan_string delimiter (thread#push_char c) state lexbuf }

  (* Scanner called first *)

and init client state = parse
  utf8_bom { state#mk_bom lexbuf                       }
| _        { rollback lexbuf; scan client state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let mk_scan (client: 'token client) =
  let internal_client : 'token internal_client =
    let open Utils in
    object
      method mk_string                = client#mk_string
      method mk_eof                   = drop <@ client#mk_eof
      method callback                 = drop <@ client#callback
      method support_string_delimiter = client#support_string_delimiter
    end
  and first_call = ref true in
  fun state ->
    let scanner =
      if !first_call then (first_call := false; init) else scan
    in lift (scanner internal_client state)

(* END TRAILER *)
}
