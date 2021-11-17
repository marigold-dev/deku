(* Driving the preprocessor *)

module Make (Comments : Comments.S) (File : File.S) =
  struct
    module Preproc_CLI = Preprocessor.CLI.Make (Comments)
    module Main        = Preprocessor.PreprocMainGen
    module Preproc     = Main.Make (Preproc_CLI)

    (* All exits *)

    let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

    let red_exit msg = print_in_red msg; exit 1

    let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

    let check_cli = Preproc.check_cli

    let preproc () : unit =
      let () =
        match Preproc_CLI.extension with
          Some ext when ext <> File.extension ->
            let msg =
              Printf.sprintf "Expected extension %s." File.extension
            in cli_error msg
        | _ -> ()
      in match Preproc.preprocess () with
           Stdlib.Ok (buffer, _) ->
             Printf.printf "%s%!" (Buffer.contents buffer)
         | _ -> ()
  end
