(* Standalone preprocessor with default settings *)

module Comments =
  struct
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    let block = None
    let line  = None
  end

module CLI     = Preprocessor.CLI.Make (Comments)
module MainGen = Preprocessor.PreprocMainGen
module Main    = MainGen.Make (CLI)

let () = Main.check_cli ()
let () = Main.preprocess () |> ignore
