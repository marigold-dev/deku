(* Interfacing the JsLIGO lexer. *)

(* LIGO dependencies *)

module Comments = Preprocessing_jsligo.Comments

(* Internal dependencies *)

module Token = Lexing_jsligo.Token

include Lexing_shared.Common.Make (Comments) (Token)
