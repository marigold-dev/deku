(* Driver for the Michelson lexer *)

(* Vendor dependencies *)
module Region = Simple_utils.Region

(* Internal dependencies *)

module Comments = Michelson.Comments
module File     = Michelson.File
module Token    = Michelson.Token

(* Reading the CLI *)

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module CLI   = LexerLib.CLI.Make (Preproc_CLI)

(* All exits *)

let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

let red_exit msg = print_in_red msg; exit 1

let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

let print_and_quit msg = print_string msg; flush stdout; exit 0

(* Checking for errors and valid exits from the CLI status *)

let check_cli () =
  match CLI.status with
    `SyntaxError  msg
  | `FileNotFound msg -> cli_error msg
  | `Help         buf
  | `CLI          buf -> print_and_quit (Buffer.contents buf)
  | `Version      ver -> print_and_quit (ver ^ "\n")
  | `Conflict (o1,o2) ->
       cli_error (Printf.sprintf "Choose either %s or %s." o1 o2)
  | `Done ->
       match CLI.Preproc_CLI.extension with
         Some ext when ext <> File.extension ->
           let msg =
             Printf.sprintf "Expected extension %s." File.extension
           in cli_error msg
       | _ -> ()

(* Instantiation of the Michelson lexer *)

type token = Token.t

module Lexer = Michelson.Lexer.Make (Token)
module Scan  = LexerLib.API.Make (Lexer)

let config : token LexerLib.Core.config =
  object
    method block     = CLI.Preproc_CLI.block
    method line      = CLI.Preproc_CLI.line
    method input     = CLI.Preproc_CLI.input
    method offsets   = CLI.Preproc_CLI.offsets
    method mode      = CLI.mode
    method command   = CLI.command
    method is_eof    = Token.is_eof
    method to_region = Token.to_region
    method to_lexeme = Token.to_lexeme
    method to_string = Token.to_string
  end

(* Scanning one token at a time with or without a preprocessor *)

type window = <
  last_token    : token option;
   current_token : token           (* Including EOF *)
>

type message = string Region.reg

let get_window = ref (fun () -> None)
let reader     = ref None

let scan lexbuf =
  match !reader with
    Some reader ->
      reader lexbuf
  | None ->
     match Scan.from_lexbuf config lexbuf with
       Stdlib.Ok LexerLib.Core.{read; get_win; _} ->
       begin
         get_window := get_win;
         reader := Some read;
         read lexbuf
       end
     | Error _ as err -> err

(* Scanning all tokens with or without a preprocessor *)

module Preproc = Preprocessor.PreprocMainGen.Make (CLI.Preproc_CLI)

let preprocess () : (Lexing.lexbuf, unit) Stdlib.result option =
  if CLI.preproc then
    match Preproc.preprocess () with
      Error (Some buffer, Region.{value; _}) ->
        if CLI.Preproc_CLI.show_pp then
          Printf.printf "%s%!" (Buffer.contents buffer)
        else ();
        print_in_red value;
        Some (Stdlib.Error ())
    | Error (None, Region.{value; _}) ->
        print_in_red value;
        Some (Stdlib.Error ())
    | Stdlib.Ok buffer ->
        if CLI.Preproc_CLI.show_pp then
          Printf.printf "%s%!" (Buffer.contents buffer)
        else ();
        let string = Buffer.contents buffer in
        Some (Ok (Lexing.from_string string))
  else None

let scan_all () : unit =
  match preprocess () with
    None ->
      let tokens =
        match config#input with
          Some path -> Scan.all_from_file config path
        |      None -> Scan.all_from_channel config stdin
      in (match tokens with
            Stdlib.Ok _ -> ()
          | Error msg -> print_in_red msg.Region.value)
  | Some Error () -> ()
  | Some Ok lexbuf ->
      let all_from_lexbuf = Scan.all_from_lexbuf config in
      match all_from_lexbuf lexbuf with
        Stdlib.Ok _ -> ()
      | Error Region.{value; _} -> print_in_red value
