(* Interfacing the CameLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_cameligo.Comments

(* Internal dependencies *)

module Token = Lexing_cameligo.Token

include Lexing_shared.Common.Make (Comments) (Token)
