(* This file provides an interface to the CameLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_jsligo.File
module Comments    = Preprocessing_jsligo.Comments
module Token       = Lexing_jsligo.Token
module Self_tokens = Lexing_jsligo.Self_tokens
module ParErr      = Parsing_jsligo.ParErr
module Parser      = Parsing_jsligo.Parser
module CST         = Cst_jsligo.CST
module Pretty      = Parsing_jsligo.Pretty

(* Making the parsers *)

module JsligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_jsligo.RecoverParser
  end

include Parsing_shared.Common.MakeTwoParsers
          (File) (Comments) (Token) (ParErr) (Self_tokens)
          (CST) (JsligoParser)

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

let pretty_print_file ~raise buffer file_path =
  ContractParser.parse_file ~raise buffer file_path |> pretty_print

let pretty_print_cst ~raise buffer file_path =
  let cst = parse_file ~raise buffer file_path in
  let buffer = Buffer.create 59 in
  let state =
    Cst_jsligo.Printer.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  let apply tree =
    Cst_jsligo.Printer.pp_cst state tree; buffer
  in apply cst
