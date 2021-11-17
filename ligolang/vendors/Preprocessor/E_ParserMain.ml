(* Standalone parser for booleans expression of preprocessing
   directives. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Internal dependencies *)

module E_Lexer  = Preprocessor.E_Lexer
module E_Parser = Preprocessor.E_Parser
module E_AST    = Preprocessor.E_AST

(* All exits *)

let red_exit msg =
  Printf.eprintf "\027[31m%s\027[0m%!" msg; exit 1

let sys_error    = red_exit
let lexing_error = red_exit
let parse_error  = red_exit

let cli_error msg =
  red_exit (Printf.sprintf "Command-line error: %s\n" msg)

let print_and_quit msg =
  print_string msg; flush stdout; exit 0

(* Instantiating and parsing the CLI for options *)

module Comments =
  struct
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    let mk_block ~opening ~closing =
      object
        method opening = opening
        method closing = closing
      end

    let line = None
    let block = None
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

(* The parser *)

let parse in_chan =
  let buffer = Lexing.from_channel in_chan in
  let () =
    match CLI.input with
      Some "-" | None -> ()
    | Some pos_fname ->
        let open Lexing in
        buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
    let () =
      try
        let tree  = E_Parser.expr E_Lexer.scan buffer in
        let value = E_AST.(eval Env.empty tree)
        in Printf.printf "%s\n" (string_of_bool value)
      with
        E_Lexer.Error error ->
          lexing_error error.Region.value
    in close_in in_chan

(* Calling the parser *)

let () =
  match CLI.input with
    Some "-" | None -> parse stdin
  | Some file_path ->
     try open_in file_path |> parse with
       Sys_error msg -> sys_error msg
