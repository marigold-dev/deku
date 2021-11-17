(* Interfacing the JsLIGO preprocessor. *)

module File     = Preprocessing_jsligo.File
module Comments = Preprocessing_jsligo.Comments

include Preprocessing_shared.Common.Make (File) (Comments)
