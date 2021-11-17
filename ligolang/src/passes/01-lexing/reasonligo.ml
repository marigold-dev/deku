(* Interfacing the ReasonLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_reasonligo.Comments

(* Internal dependencies *)

module Token = Lexing_reasonligo.Token

include Lexing_shared.Common.Make (Comments) (Token)
