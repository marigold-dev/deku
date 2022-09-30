open Deku_stdlib
open Deku_storage
open Deku_chain

type params = {
  folder_path : string;
      [@pos 0]
      [@docv "FOLDER_PATH"]
      [@doc "Path to the folder where chain.json is stored"]
  check_only : bool;
      [@default false]
      [@doc
        "If true, only deserializes the state as an integrity check and does \
         not write any new files."]
}
[@@deriving cmdliner]

(* TODO:
   bake the current git revision into the binary with nix so we can report which
   version the state is being check against.
*)
(* FIXME: loop over a json array of transactions passed via 'content' *)
let main { folder_path; check_only } =
  Eio_main.run @@ fun env ->
  let () =
    Format.printf "Reading %s/chain.json. This could take a while...\n%!"
      folder_path
  in
  let chain = Storage.Chain.read ~env ~folder:folder_path |> Option.get in
  if check_only then print_endline "chain.json deserializes correctly 🤘"
  else print_endline "Pruning state";
  let chain = Chain.prune chain in
  let bin = Chain.yojson_of_t chain |> Yojson.Safe.to_string in
  let cwd = Eio.Stdenv.cwd env in
  let file = Eio.Path.(cwd / folder_path / "pruned_chain.json") in
  Eio.Path.save ~create:(`If_missing 0o644) file bin

let () =
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
