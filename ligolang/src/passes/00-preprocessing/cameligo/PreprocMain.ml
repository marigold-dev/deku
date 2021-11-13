(* Driving the preprocessor for CameLIGO *)

module Comments       = Preprocessing_cameligo.Comments
module File           = Preprocessing_cameligo.File
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
