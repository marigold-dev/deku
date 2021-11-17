(* Interfacing the PascaLIGO preprocessor. *)

module File     = Preprocessing_pascaligo.File
module Comments = Preprocessing_pascaligo.Comments

include Preprocessing_shared.Common.Make (File) (Comments)
