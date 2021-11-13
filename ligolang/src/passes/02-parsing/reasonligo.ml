(* This file provides an interface to the ReasonLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_reasonligo.File
module Comments    = Preprocessing_reasonligo.Comments
module Token       = Lexing_reasonligo.Token
module Self_tokens = Lexing_reasonligo.Self_tokens
module ParErr      = Parsing_reasonligo.ParErr
module Parser      = Parsing_reasonligo.Parser
module CST         = Cst_reasonligo.CST
module Pretty      = Parsing_reasonligo.Pretty

(* Making the parsers *)

module ReasonligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_reasonligo.RecoverParser
  end

include Parsing_shared.Common.MakeTwoParsers
          (File) (Comments) (Token) (ParErr) (Self_tokens)
          (CST) (ReasonligoParser)

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

let pretty_print_file ~raise buffer file_path =
  ContractParser.parse_file ~raise buffer file_path |> pretty_print

let pretty_print_cst ~raise buffer file_path =
  let cst = parse_file ~raise buffer file_path in
  let buffer = Buffer.create 59 in
  let state =
    Cst_reasonligo.Printer.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  let apply tree =
    Cst_reasonligo.Printer.pp_cst state tree; buffer
  in apply cst
