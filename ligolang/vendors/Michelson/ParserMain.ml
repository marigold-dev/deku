(* Driver for the parser of Michelson *)

open! EvalOpt (* Reads the command-line options: Effectful! *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

(* Running the parser on the input file *)

module Lexer = Lexer.Make (Token)

let instance = Lexer.open_token_stream EvalOpt.input

let log = Lexer.output_token ~offsets:EvalOpt.offsets
            EvalOpt.mode EvalOpt.cmd stdout

let scanner buffer = instance.read ~log buffer

let () =
  try
    Parser.program scanner instance.buffer
  with Parser.Error -> ()
