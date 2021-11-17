(* Interfacing the ReasonLIGO preprocessor. *)

module File     = Preprocessing_reasonligo.File
module Comments = Preprocessing_reasonligo.Comments

include Preprocessing_shared.Common.Make (File) (Comments)
