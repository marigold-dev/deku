(* Interfacing the PascaLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_pascaligo.Comments

(* Internal dependencies *)

module Token = Lexing_pascaligo.Token

include Lexing_shared.Common.Make (Comments) (Token)
