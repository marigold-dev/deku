(* Driver for the parser of PascaLIGO *)

open SParser (* TEMPORARY *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

(* Reading the command-line options *)

let options = EvalOpt.read "PascaLIGO" ".ligo"

open EvalOpt

(* Auxiliary functions *)

let sprintf = Printf.sprintf

(* Extracting the input file *)

let file =
  match options.input with
    None | Some "-" -> false
  |         Some _  -> true

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

type Error.t += ParseError

let error_to_string = function
  ParseError -> "Syntax error.\n"
| _ -> assert false

let print_error ?(offsets=true) mode Region.{region; value} ~file =
  let  msg = error_to_string value in
  let  reg = region#to_string ~file ~offsets mode in
  Utils.highlight (sprintf "Parse error %s:\n%s%!" reg msg)

(* Path for CPP inclusions (#include) *)

let lib_path =
  match options.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""

(* Preprocessing the input source and opening the input channels *)

let prefix =
  match options.input with
    None | Some "-" -> "temp"
  | Some file ->  Filename.(file |> basename |> remove_extension)

let suffix = ".pp.ligo"

let pp_input =
  if Utils.String.Set.mem "cpp" options.verbose
  then prefix ^ suffix
  else let pp_input, pp_out = Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  match options.input with
    None | Some "-" ->
      Printf.sprintf "cpp -traditional-cpp%s - > %s"
                     lib_path pp_input
  | Some file ->
      Printf.sprintf "cpp -traditional-cpp%s %s > %s"
                     lib_path file pp_input

let () =
  if Utils.String.Set.mem "cpp" options.verbose
  then Printf.eprintf "%s\n%!" cpp_cmd;
  if Sys.command cpp_cmd <> 0 then
    external_ (Printf.sprintf "the command \"%s\" failed." cpp_cmd)

(* Instanciating the lexer *)

module Lexer = Lexer.Make (LexToken)

module Log = LexerLog.Make (Lexer)

let Lexer.{read; buffer; get_pos; get_last; close} =
  Lexer.open_token_stream (Some pp_input)

and cout = stdout

let log = Log.output_token ~offsets:options.offsets
                           options.mode options.cmd cout

and close_all () = close (); close_out cout

(* Tokeniser *)

let tokeniser = read ~log

(* Main *)

let () =
  try
    let ast = Parser.contract tokeniser buffer in
    if Utils.String.Set.mem "ast" options.verbose
    then let buffer = Buffer.create 131
         in begin
              Printer.offsets := options.offsets;
              Printer.mode    := options.mode;
              Printer.print_tokens buffer ast;
              Buffer.output_buffer stdout buffer
         end
  with
    Lexer.Error err ->
      close_all ();
      Lexer.print_error ~offsets:options.offsets
                        options.mode err ~file
  | Parser.Error ->
      let region = get_last () in
      let error = Region.{region; value=ParseError} in
      let () = close_all () in
      print_error ~offsets:options.offsets
                  options.mode error ~file
  | Sys_error msg -> Utils.highlight msg
