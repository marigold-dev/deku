(* Converting the textual representation of tokens produced by Menhir
   into concrete syntax *)

module Token = Lexing_reasonligo.Token
module Main  = Lexing_shared.UnlexerGen.Make (Token)

(* Reading one line from the standard input channel and unlex it. *)

let out = Main.unlex (input_line stdin) |> Buffer.contents
let ()  = Printf.printf "%s\n" out
