(* Simple preprocessor based on C#, to be processed by [ocamllex]. *)

{
(* START OF HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}

(* Utility functions *)

let sprintf = Printf.sprintf

(* STRING PROCESSING *)

(* The value of [mk_str len p] ("make string") is a string of length
   [len] containing the [len] characters in the list [p], in reverse
   order. For instance, [mk_str 3 ['c';'b';'a'] = "abc"]. *)

let mk_str (len: int) (p: char list) : string =
  let () = assert (len = List.length p) in
  let bytes = Bytes.make len ' ' in
  let rec fill i = function
    [] -> bytes
  | char::l -> Bytes.set bytes i char; fill (i-1) l
  in fill (len-1) p |> Bytes.to_string

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif and
   #else. *)

type cond  = If of mode | Elif of mode | Else
type trace = cond list

(* The type [state] groups the information that needs to be
   threaded along the scanning functions:
     * the field [cfg] records the source configuration;
     * the field [dirs] contains the file paths given in "-I";
     * the field [env] records the symbols defined;
     * the field [mode] informs whether the preprocessor is in
       copying or skipping mode;
     * the field [trace] is a stack of previous, still active
       conditional directives;
     * the field [out] keeps the output buffer;
     * the field [chans] is a list of opened input channels
       (#include);
     * the field [incl] is the file system's path to the the
       current input file;
     * the field [import] is a list of (filename, module) imports
       (#import);
     *)

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type file_path = string
type module_name = string

type config = <
  block   : block_comment option;
  line    : line_comment option;
  input   : file_path option;
  offsets : bool;
  dirs    : file_path list (* Directories to search for #include files *)
>

type state = {
  config : config;
  env    : E_AST.Env.t;
  mode   : mode;
  trace  : trace;
  out    : Buffer.t;
  chans  : in_channel list;
  incl   : file_path list;
  import : (file_path * module_name) list;
}

(* Directories *)

let push_dir dir state =
  if dir = "." then state else {state with incl = dir :: state.incl}

let mk_path state =
  String.concat Filename.dir_sep (List.rev state.incl)

(* ERRORS *)

type error =
  Directive_inside_line
| Missing_endif
| Newline_in_string
| Unterminated_string
| Dangling_endif
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Reserved_symbol of string
| Multiply_defined_symbol of string
| Error_directive of string           (* #error ONLY *)
| Parse_error
| Invalid_symbol
| File_not_found of string
| Unterminated_comment of string
| Missing_filename                    (* #include *)
| Unexpected_argument                 (* #include and #import *)

let error_to_string = function
  Directive_inside_line ->
    sprintf "Directive inside a line."
| Missing_endif ->
    sprintf "Missing #endif directive."
| Newline_in_string ->
    sprintf "Invalid newline character in string."
| Unterminated_string ->
    sprintf "Unterminated string.\n\
             Hint: Close with double quotes."
| Dangling_endif ->
    sprintf "Dangling #endif directive.\n\
             Hint: Remove it or add a #if before."
| If_follows_elif ->
    sprintf "Directive #if found in a clause #elif."
| Else_follows_else ->
    sprintf "Directive #else found in a clause #else."
| Dangling_else ->
    sprintf "Directive #else without #if."
| Elif_follows_else ->
    sprintf "Directive #elif found in a clause #else."
| Dangling_elif ->
    sprintf "Dangling #elif directive.\n\
             Hint: Remove it or add a #if before."
| Reserved_symbol sym ->
    sprintf "Reserved symbol %S.\n\
             Hint: Use another symbol." sym
| Multiply_defined_symbol sym ->
    sprintf "Multiply-defined symbol %S.\n\
             Hint: Change the name or remove one definition." sym
| Error_directive msg ->
    if msg = "" then sprintf "Directive #error reached." else msg
| Parse_error ->
    "Parse error in expression."
| Invalid_symbol ->
   "Expected a symbol (identifier)."
| File_not_found name ->
    sprintf "File %S to include not found." name
| Unterminated_comment ending ->
    sprintf "Unterminated comment.\n\
             Hint: Close with %S." ending
| Missing_filename ->
    sprintf "Filename expected in a string literal."
| Unexpected_argument ->
    sprintf "Unexpected argument."

let format_error config ~msg (region: Region.t) =
  let file  = config#input <> None in
  let reg   = region#to_string
                ~file
                ~offsets:config#offsets
                `Byte in
  let value = sprintf "%s:\n%s\n" reg msg
  in Region.{value; region}

(* IMPORTANT : Make sure the functions [fail] and [expr] remain the
   only ones raising [Error]. *)

exception Error of (Buffer.t * string Region.reg)

let fail state region error =
  let msg = error_to_string error in
  let msg = format_error state.config ~msg region
  in List.iter close_in state.chans;
     raise (Error (state.out, msg))

let mk_reg buffer =
  let start = Lexing.lexeme_start_p buffer |> Pos.from_byte
  and stop  = Lexing.lexeme_end_p buffer |> Pos.from_byte
  in Region.make ~start ~stop

let stop state buffer = fail state (mk_reg buffer)

(* The function [reduce_cond] is called when a #endif directive is
   found, and the trace (see type [trace] above) needs updating. *)

let reduce_cond state region =
  let rec reduce = function
                [] -> fail state region Dangling_endif
  | If mode::trace -> {state with mode; trace}
  |       _::trace -> reduce trace
  in reduce state.trace

(* The function [extend] is called when encountering conditional
   directives #if, #else and #elif. As its name suggests, it extends
   the current trace with the current conditional directive, whilst
   performing some validity checks. *)

let extend cond state region =
  match cond, state.trace with
    If _, Elif _::_ -> fail state region If_follows_elif
  | Else,   Else::_ -> fail state region Else_follows_else
  | Else,        [] -> fail state region Dangling_else
  | Elif _, Else::_ -> fail state region Elif_follows_else
  | Elif _,      [] -> fail state region Dangling_elif
  |     hd,     tl  -> hd::tl

(* The function [last_mode] seeks the last mode as recorded in the
   trace (see type [trace] above). *)

let rec last_mode = function
                        [] -> assert false
| (If mode | Elif mode)::_ -> mode
|                 _::trace -> last_mode trace

(* Finding a file to #include *)

let rec find file_path = function
         [] -> None
| dir::dirs ->
    let path =
      if dir = "." || dir = "" then file_path
      else dir ^ Filename.dir_sep ^ file_path in
    try Some (path, open_in path) with
      Sys_error _ -> find file_path dirs

let find dir file dirs =
  let path =
    if dir = "." || dir = "" then file
    else dir ^ Filename.dir_sep ^ file in
  try Some (path, open_in path) with
    Sys_error _ ->
      let base = Filename.basename file in
      if base = file then find file dirs else None

(* PRINTING *)

(* Copying the current lexeme to [stdout] *)

let copy state buffer =
  Buffer.add_string state.out (Lexing.lexeme buffer)

(* End of lines are always copied. ALWAYS AND ONLY USE AFTER SCANNING nl. *)

let proc_nl state buffer =
  Lexing.new_line buffer; copy state buffer

(* Copying a string *)

let print state string = Buffer.add_string state.out string

(* Evaluating a preprocessor expression

   The evaluation of conditional directives may involve symbols whose
   value may be defined using #define directives, or undefined by
   means of #undef. Therefore, we need to evaluate conditional
   expressions in an environment made of a set of defined symbols.

     Note that we rely on an external lexer and parser for the
   conditional expressions. See modules [E_AST], [E_Lexer] and
   [E_Parser]. *)

let expr state buffer : mode =
  let ast =
    try E_Parser.expr E_Lexer.scan buffer with
      E_Lexer.Error e -> raise (Error (state.out, e))
    | E_Parser.Error  -> stop state buffer Parse_error in
  let () = print state "\n" in
  if E_AST.eval state.env ast then Copy else Skip

(* DIRECTIVES *)

let directives = [
  "define";
  "elif";
  "else";
  "endif";
  "error";
  "if";
  "import";
  "include";
  "undef"
]

(* END OF HEADER *)
}

(* REGULAR EXPRESSIONS *)

let nl        = '\n' | '\r' | "\r\n"
let blank     = ' ' | '\t'
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let small     = ['a'-'z']
let capital   = ['A'-'Z']
let letter    = small | capital
let ident     = letter (letter | '_' | digit)*
let directive = '#' blank* (small+ as id)

(* Comments *)

let pascaligo_block_comment_opening = "(*"
let pascaligo_block_comment_closing = "*)"
let pascaligo_line_comment          = "//"

let cameligo_block_comment_opening = "(*"
let cameligo_block_comment_closing = "*)"
let cameligo_line_comment          = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment          = "//"

let michelson_block_comment_opening = "/*"
let michelson_block_comment_closing = "*/"
let michelson_line_comment          = "#"

let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening

let block_comment_closings =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing
| michelson_block_comment_closing

let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment

(* Rules *)

(* The rule [scan] scans the input buffer for directives, strings,
   comments, blanks, new lines and end of file characters. As a
   result, either the matched input is copied to [stdout] or not,
   depending on the compilation directives. If not copied, new line
   characters are output.

   Scanning is triggered by the function call [scan env mode trace
   lexbuf], where [env] is the set of defined symbols (introduced by
   `#define'), [mode] specifies whether we are copying or skipping the
   input, and [trace] is the stack of conditional directives read so
   far.

   The first call is [scan {env=Env.empty; mode=Copy; trace=[];
   incl=[]}], meaning that we start with an empty environment, that
   copying the input is enabled by default, and that we are at the
   start of a line and no previous conditional directives have been
   read yet.

   When an "#if" is matched, the trace is extended by the call [extend
   lexbuf (If mode) trace], during the evaluation of which the
   syntactic validity of having encountered an "#if" is checked (for
   example, it would be invalid had an "#elif" been last read). Note
   that the current mode is stored in the trace with the current
   directive -- that mode may be later restored (see below for some
   examples). Moreover, the directive would be deemed invalid if its
   current position in the line (that is, its offset) were not
   preceeded by blanks or nothing, otherwise the rule [expr] is called
   to scan the boolean expression associated with the "#if": if it
   evaluates to [true], the result is [Copy], meaning that we may copy
   what follows, otherwise skip it -- the actual decision depending on
   the current mode. That new mode is used if we were in copy mode,
   and the offset is reset to the start of a new line (as we read a
   new line in [expr]); otherwise we were in skipping mode and the
   value of the conditional expression must be ignored (but not its
   syntax), and we continue skipping the input.

   When an "#else" is matched, the trace is extended with [Else], then
   the rest of the line is scanned with [skip_line]. If we were in
   copy mode, the new mode toggles to skipping mode; otherwise, the
   trace is searched for the last encountered "#if" of "#elif" and the
   associated mode is restored.

   The case "#elif" is the result of the fusion (in the technical
   sense) of the code for dealing with an "#else" followed by an
   "#if".

   When an "#endif" is matched, the trace is reduced, that is, all
   conditional directives are popped until an [If mode'] is found and
   [mode'] is restored as the current mode.

   Consider the following four cases, where the modes (Copy/Skip) are
   located between the lines:

                    Copy ----+                          Copy ----+
   #if true                  |       #if true                    |
                    Copy     |                          Copy     |
   #else                     |       #else                       |
                +-- Skip --+ |                      +-- Skip --+ |
     #if true   |          | |         #if false    |          | |
                |   Skip   | |                      |   Skip   | |
     #else      |          | |         #else        |          | |
                +-> Skip   | |                      +-> Skip   | |
     #endif                | |         #endif                  | |
                    Skip <-+ |                          Skip <-+ |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+


                +-- Copy ----+                          Copy --+-+
   #if false    |            |       #if false                 | |
                |   Skip     |                          Skip   | |
   #else        |            |       #else                     | |
                +-> Copy --+ |                    +-+-- Copy <-+ |
     #if true              | |         #if false  | |            |
                    Copy   | |                    | |   Skip     |
     #else                 | |         #else      | |            |
                    Skip   | |                    | +-> Copy     |
     #endif                | |         #endif     |              |
                    Copy <-+ |                    +---> Copy     |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+

   The following four cases feature #elif. Note that we put between
   brackets the mode saved for the #elif, which is sometimes restored
   later.

                    Copy --+                            Copy --+
   #if true                |         #if true                  |
                    Copy   |                            Copy   |
   #elif true   +--[Skip]  |         #elif false    +--[Skip]  |
                |   Skip   |                        |   Skip   |
   #else        |          |         #else          |          |
                +-> Skip   |                        +-> Skip   |
   #endif                  |         #endif                    |
                    Copy <-+                            Copy <-+


                +-- Copy --+-+                      +-- Copy ----+
   #if false    |          | |       #if false      |            |
                |   Skip   | |                      |   Skip     |
   #elif true   +->[Copy]  | |       #elif false    +->[Copy]--+ |
                    Copy <-+ |                          Skip   | |
   #else                     |       #else                     | |
                    Skip     |                          Copy <-+ |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+

   Note how "#elif" indeed behaves like an "#else" followed by an
   "#if", and the mode stored with the data constructor [Elif]
   corresponds to the mode before the virtual "#if".

   Important note: Comments and strings are recognised as such only in
   copy mode, which is a different behaviour from the preprocessor of
   GNU GCC, which always does.
 *)

rule scan state = parse
  nl    { proc_nl state lexbuf; scan state lexbuf }
| blank { if state.mode = Copy then copy state lexbuf;
          scan state lexbuf }
| directive {
    let  region = mk_reg lexbuf in
    if   not (List.mem id directives)
    then begin
           if state.mode = Copy then copy state lexbuf;
           scan state lexbuf
         end
    else
    if   region#start#offset `Byte > 0
    then stop state lexbuf Directive_inside_line
    else
    match id with
      "include" ->
        let line = Lexing.(lexbuf.lex_curr_p.pos_lnum)
        and file = Lexing.(lexbuf.lex_curr_p.pos_fname) in
        let base = Filename.basename file
        and reg, incl_file = scan_include state lexbuf in
        if state.mode = Copy then
          let incl_dir = Filename.dirname incl_file in
          let path = mk_path state in
          let incl_path, incl_chan =
            match find path incl_file state.config#dirs with
              Some p -> p
            |   None -> fail state reg (File_not_found incl_file) in
          let () = print state (sprintf "\n# 1 %S 1\n" incl_path) in
          let incl_buf = Lexing.from_channel incl_chan in
          let () =
            let open Lexing in
            incl_buf.lex_curr_p <-
              {incl_buf.lex_curr_p with pos_fname = incl_file} in
          let state  = {state with chans = incl_chan::state.chans} in
          let state' = {state with mode=Copy; trace=[]} in
          let state' = scan (push_dir incl_dir state') incl_buf in
          let state  = {state with env=state'.env; chans=state'.chans;import=state'.import} in
          let path   = if path = "" || path = "." then base
                       else path ^ Filename.dir_sep ^ base in
          let ()     = print state (sprintf "\n# %i %S 2\n" (line+1) path)
          in scan state lexbuf
        else scan state lexbuf
    | "import" ->
        let reg, import_file, imported_module = scan_import state lexbuf in
        if state.mode = Copy then
          let path = mk_path state in
          let import_path =
            match find path import_file state.config#dirs with
              Some p -> fst p
            | None -> fail state reg (File_not_found import_file) in
          let state  = {state with
                         import = (import_path, imported_module)::state.import}
          in scan state lexbuf
        else scan state lexbuf
    | "if" ->
        let mode  = expr state lexbuf in
        let mode  = if state.mode = Copy then mode else Skip in
        let trace = extend (If state.mode) state region in
        let state = {state with mode; trace}
        in scan state lexbuf
    | "else" ->
        let ()    = skip_line state lexbuf in
        let mode  = match state.mode with
                      Copy -> Skip
                    | Skip -> last_mode state.trace in
        let trace = extend Else state region
        in scan {state with mode; trace} lexbuf
    | "elif" ->
        let mode = expr state lexbuf in
        let trace, mode =
          match state.mode with
            Copy -> extend (Elif Skip) state region, Skip
          | Skip -> let old_mode = last_mode state.trace
                    in extend (Elif old_mode) state region,
                       if old_mode = Copy then mode else Skip
        in scan {state with mode; trace} lexbuf
    | "endif" ->
        skip_line state lexbuf;
        scan (reduce_cond state region) lexbuf
    | "define" ->
        let id, region = variable state lexbuf in
        if state.mode = Copy then
          if id="true" || id="false"
          then fail state region (Reserved_symbol id)
          else
            if E_AST.Env.mem id state.env
            then fail state region (Multiply_defined_symbol id)
            else
              let state = {state with env = E_AST.Env.add id state.env}
              in scan state lexbuf
        else scan state lexbuf
    | "undef" ->
        let id, _ = variable state lexbuf in
        if state.mode = Copy then
          let state = {state with env = E_AST.Env.remove id state.env}
          in scan state lexbuf
        else scan state lexbuf
    | "error" ->
        fail state region (Error_directive (message [] lexbuf))
    | _ -> assert false }

| eof { if state.trace = [] then state
        else stop state lexbuf Missing_endif }

| '"' { if state.mode = Copy then
          begin
            copy state lexbuf;
            scan (in_string (mk_reg lexbuf) state lexbuf) lexbuf
          end
        else scan state lexbuf }

| block_comment_openings {
    let lexeme = Lexing.lexeme lexbuf in
    match state.config#block with
      Some block when block#opening = lexeme ->
        if state.mode = Copy then
          begin
            copy state lexbuf;
            let state = in_block block (mk_reg lexbuf) state lexbuf
            in scan state lexbuf
          end
        else scan state lexbuf
    | Some _ | None ->
        let n = String.length lexeme in
          begin
            rollback lexbuf;
            assert (n > 0);
            scan (scan_n_char n state lexbuf) lexbuf
          end }

| line_comments {
    let lexeme = Lexing.lexeme lexbuf in
    match state.config#line with
      Some line when line = lexeme ->
        if state.mode = Copy then
          begin
            copy state lexbuf;
            scan (in_line_com state lexbuf) lexbuf
          end
        else scan state lexbuf
    | Some _ | None ->
        let n = String.length lexeme in
          begin
            rollback lexbuf;
            assert (n > 0);
            scan (scan_n_char n state lexbuf) lexbuf
          end }

| _ { if state.mode = Copy then copy state lexbuf;
      scan state lexbuf }

(* Scanning a series of characters *)

and scan_n_char n state = parse
  _  { if state.mode = Copy then copy state lexbuf;
       if n = 1 then state else scan_n_char (n-1) state lexbuf }

(* Support for #define and #undef *)

and variable state = parse
  blank+ { let id = symbol state lexbuf
           in skip_line state lexbuf; id }

and symbol state = parse
  ident as id { id, mk_reg lexbuf                }
| _           { stop state lexbuf Invalid_symbol }

(* Skipping all characters until the end of line or end of file. *)

and skip_line state = parse
  nl     { proc_nl state lexbuf   }
| eof    { rollback lexbuf        }
| _      { skip_line state lexbuf }

(* For #error, #region and #endregion *)

and message acc = parse
  nl     { Lexing.new_line lexbuf;
           mk_str (List.length acc) acc }
| eof    { rollback lexbuf;
           mk_str (List.length acc) acc }
| blank* { message acc lexbuf           }
| _ as c { message (c::acc) lexbuf      }

(* Comments *)

and in_line_com state = parse
  nl  { proc_nl state lexbuf; state                  }
| eof { rollback lexbuf; state              }
| _   { if state.mode = Copy then copy state lexbuf;
        in_line_com state lexbuf                     }

and in_block block opening state = parse
  '"' | block_comment_openings {
    let lexeme = Lexing.lexeme lexbuf in
    if   block#opening = lexeme || lexeme = "\""
    then let ()       = copy state lexbuf in
         let opening' = mk_reg lexbuf in
         let next     = if lexeme = "\"" then in_string
                        else in_block block in
         let state    = next opening' state lexbuf
         in in_block block opening state lexbuf
    else let ()    = rollback lexbuf in
         let n     = String.length lexeme in
         let ()    = assert (n > 0) in
         let state = scan_n_char n state lexbuf
         in in_block block opening state lexbuf }

| block_comment_closings {
    let lexeme = Lexing.lexeme lexbuf in
    if   block#closing = lexeme
    then (copy state lexbuf; state)
    else let ()    = rollback lexbuf in
         let n     = String.length lexeme in
         let ()    = assert (n > 0) in
         let state = scan_n_char n state lexbuf
         in in_block block opening state lexbuf }

| nl   { proc_nl state lexbuf; in_block block opening state lexbuf }
| eof  { let err = Unterminated_comment block#closing
         in fail state opening err                                 }
| _    { copy state lexbuf; in_block block opening state lexbuf    }

(* #include *)

and scan_include state = parse
  blank+ { scan_include state lexbuf                    }
| '"'    { in_include (mk_reg lexbuf) [] 0 state lexbuf }
| _      { stop state lexbuf Missing_filename           }

and in_include opening acc len state = parse
  '"'    { let region = Region.cover opening (mk_reg lexbuf)
           in region, end_include acc len state lexbuf       }
| nl     { stop state lexbuf Newline_in_string               }
| eof    { fail state opening Unterminated_string            }
| _ as c { in_include opening (c::acc) (len+1) state lexbuf  }

and end_include acc len state = parse
  nl     { Lexing.new_line lexbuf; mk_str len acc         }
| eof    { mk_str len acc                                 }
| blank+ { end_include acc len state lexbuf               }
| _      { fail state (mk_reg lexbuf) Unexpected_argument }

(* #import *)

and scan_import state = parse
  blank+ { scan_import state lexbuf                    }
| '"'    { in_import (mk_reg lexbuf) [] 0 state lexbuf }
| _      { stop state lexbuf Missing_filename          }

and in_import opening acc len state = parse
  '"'    { let imp_path = mk_str len acc
           in scan_module opening imp_path state lexbuf    }
| nl     { stop state lexbuf Newline_in_string             }
| eof    { fail state opening Unterminated_string          }
| _ as c { in_import opening (c::acc) (len+1) state lexbuf }

and scan_module opening imp_path state = parse
  blank+ { scan_module opening imp_path state lexbuf    }
| '"'    { in_module opening imp_path [] 0 state lexbuf }
| _      { stop state lexbuf Missing_filename           }

and in_module opening imp_path acc len state = parse
  '"'    { end_module opening (mk_reg lexbuf) imp_path acc len state lexbuf }
| nl     { stop state lexbuf Newline_in_string                              }
| eof    { fail state opening Unterminated_string                           }
| _ as c { in_module opening imp_path (c::acc) (len+1) state lexbuf         }


and end_module opening closing imp_path acc len state = parse
  nl     { proc_nl state lexbuf;
           Region.cover opening closing, imp_path, mk_str len acc   }
| eof    { Region.cover opening closing, imp_path, mk_str len acc   }
| blank+ { end_module opening closing imp_path acc len state lexbuf }
| _      { fail state (mk_reg lexbuf) Unexpected_argument           }

(* Strings *)

and in_string opening state = parse
  "\\\"" { copy state lexbuf; in_string opening state lexbuf }
| '"'    { copy state lexbuf; state                          }
| eof    { rollback lexbuf; state                            }
| _      { copy state lexbuf; in_string opening state lexbuf }

and preproc state = parse
  eof { state }
| _   { let open Lexing in
        let ()   = rollback lexbuf in
        let name = lexbuf.lex_start_p.pos_fname in
        let ()   = if name <> "" then
                     print state (sprintf "# 1 %S\n" name)
        in scan state lexbuf }

{
(* START OF TRAILER *)

(* The function [preproc] is a wrapper of [scan], which also checks
   that the trace is empty at the end.  Note that we discard the state
   at the end. *)

type module_deps = (file_path * module_name) list
type success     = Buffer.t * module_deps
type message     = string Region.reg

type result = (success, Buffer.t option * message) Stdlib.result

type 'src preprocessor = config -> 'src -> result

(* Preprocessing from various sources *)

let from_lexbuf config buffer =
  let path = Lexing.(buffer.lex_curr_p.pos_fname) in
  let state = {
    config;
    env    = E_AST.Env.empty;
    mode   = Copy;
    trace  = [];
    out    = Buffer.create 80;
    chans  = [];
    incl   = [Filename.dirname path];
    import = []
  } in
  match preproc state buffer with
    state ->
      List.iter close_in state.chans;
      Stdlib.Ok (state.out, state.import)
  | exception Error (buffer, msg) ->
      Stdlib.Error (Some buffer, msg)

let from_channel config chan =
  Lexing.from_channel chan |> from_lexbuf config

let from_string config str =
  Lexing.from_string str |> from_lexbuf config

let from_file config name =
  try
    let lexbuf = open_in name |> Lexing.from_channel in
    let open Lexing in
    begin
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname=name};
      from_lexbuf config lexbuf
    end
  with Sys_error msg ->
    Stdlib.Error (None, Region.wrap_ghost msg)

(* END OF TRAILER *)
}
