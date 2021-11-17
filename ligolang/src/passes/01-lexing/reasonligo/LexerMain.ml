(* Driver for the ReasonLIGO lexer *)

module Comments         = Preprocessing_reasonligo.Comments
module File             = Preprocessing_reasonligo.File
module Token            = Lexing_reasonligo.Token
module Preprocessor_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI        = LexerLib.CLI.Make (Preprocessor_CLI)
module Self_tokens      = Lexing_reasonligo.Self_tokens
module MainGen          = Lexing_shared.LexerMainGen
module Main = MainGen.Make (File) (Token) (Lexer_CLI) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
