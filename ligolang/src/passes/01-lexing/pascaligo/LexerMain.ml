(* Driver for the PascaLIGO lexer *)

module Comments         = Preprocessing_pascaligo.Comments
module File             = Preprocessing_pascaligo.File
module Token            = Lexing_pascaligo.Token
module Preprocessor_CLI = Preprocessor.CLI.Make (Comments)
module Lexer_CLI        = LexerLib.CLI.Make (Preprocessor_CLI)
module Self_tokens      = Lexing_pascaligo.Self_tokens
module MainGen          = Lexing_shared.LexerMainGen
module Main = MainGen.Make (File) (Token) (Lexer_CLI) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
