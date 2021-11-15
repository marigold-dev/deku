(* Driving the preprocessor for JsLIGO *)

module Comments       = Preprocessing_jsligo.Comments
module File           = Preprocessing_jsligo.File
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
