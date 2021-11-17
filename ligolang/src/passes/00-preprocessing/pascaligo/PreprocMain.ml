(* Driving the preprocessor for PascaLIGO *)

module Comments       = Preprocessing_pascaligo.Comments
module File           = Preprocessing_pascaligo.File
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
