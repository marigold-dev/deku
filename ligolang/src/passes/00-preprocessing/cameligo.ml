(* Interfacing the CameLIGO preprocessor. *)

module File     = Preprocessing_cameligo.File
module Comments = Preprocessing_cameligo.Comments

include Preprocessing_shared.Common.Make (File) (Comments)
