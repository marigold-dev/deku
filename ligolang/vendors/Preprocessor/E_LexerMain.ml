(* Standalone lexer for Boolean expressions in preprocessing
   directives *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Internal dependencies *)

module E_Lexer  = Preprocessor.E_Lexer
module E_Parser = Preprocessor.E_Parser

(* All exits *)

let red_exit msg =
  Printf.eprintf "\027[31m%s\027[0m%!" msg; exit 1

let cli_error msg =
  red_exit (Printf.sprintf "Command-line error: %s\n" msg)

let sys_error    = red_exit
let lexing_error = red_exit

let print_and_quit msg =
  print_string msg; flush stdout; exit 0

(* Instantiating and parsing the CLI for options

   NOTE: We reuse the CLI options of the preprocessor, even if not all
   options make sense here. *)

module Comments =
  struct
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    let mk_block ~opening ~closing =
      object
        method opening = opening
        method closing = closing
      end

    let block = None
    let line  = None
  end

module CLI = Preprocessor.CLI.Make (Comments)

(* Checking for errors and valid exits *)

let () =
  let open CLI in
  match status with
    `SyntaxError  msg
  | `FileNotFound msg -> cli_error msg
  | `Help         buf
  | `CLI          buf -> print_and_quit (Buffer.contents buf)
  | `Version      ver -> print_and_quit (ver ^ "\n")
  | `Done             -> ()

(* Wrapper for the lexer *)

let lex in_chan =
  let buffer = Lexing.from_channel in_chan in
  let () =
    match CLI.input with
      Some "-" | None -> ()
    | Some pos_fname ->
        let open Lexing in
        buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
  let rec iter () =
    match E_Lexer.scan buffer with
      token -> Printf.printf "%s\n" (E_Lexer.string_of_token token);
              if token <> E_Parser.EOL then iter ()
    | exception E_Lexer.Error message ->
        lexing_error message.Region.value
  in iter (); close_in in_chan

(* Calling the lexer *)

let () =
  match CLI.input with
    Some "-" | None -> lex stdin
  | Some file_path ->
     try open_in file_path |> lex with
       Sys_error msg -> sys_error msg
