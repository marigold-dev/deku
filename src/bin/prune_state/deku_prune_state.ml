open Deku_stdlib
open Deku_storage
open Deku_chain
open Deku_consensus
open Deku_gossip

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
  Lwt_main.run
  @@
  let () =
    Format.printf "Reading %s/chain.json. This could take a while...\n%!"
      folder_path
  in
  let%await chain_opt = Storage.Chain.read ~folder:folder_path in
  if check_only then (
    print_endline "chain.json deserializes correctly 🤘";
    Lwt.return ())
  else
    let (Chain.Chain_data { consensus; protocol; producer; _ }) =
      Option.get chain_opt
    in
    print_endline "Pruning state";
    let (Consensus_data { current_block; validators; last_update; _ }) =
      consensus
    in
    let current_block = current_block in
    let consensus =
      Consensus.Consensus_data
        {
          current_block;
          validators;
          last_update;
          accepted = Block_hash.Set.empty;
          block_pool = Block_pool.empty;
        }
    in
    let gossip = Gossip.empty in
    let chain =
      Chain.Chain_data
        {
          gossip;
          protocol;
          consensus;
          producer;
          applied = Block_hash.Map.empty;
        }
    in
    let bin = Chain.yojson_of_chain_data chain |> Yojson.Safe.to_string in
    let file = Filename.concat folder_path "pruned_chain.json" in
    let () = Format.printf "Writing %s\n%!" file in
    Lwt_io.with_file ~mode:Output file (fun oc ->
        let%await () = Lwt_io.write oc bin in
        Lwt_io.close oc)

let () =
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
