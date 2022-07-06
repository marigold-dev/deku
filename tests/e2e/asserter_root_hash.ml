(* TODO: Make e2e tests to verify whether the Update_root_hash module
   in consensus smart contract works properly *)

open Cmdliner
open Bin_common
open Crypto

let assert_root_hash data_folder root_hash =
  let file = data_folder ^ "/state.bin" in
  let protocol_state = Files.State_bin.read ~file |> Lwt_main.run in
  let state_root_hash = protocol_state.state_root_hash |> BLAKE2B.to_string in
  assert (state_root_hash = root_hash);
  Format.printf "The root hash is updated properly 👍 \n"

let args =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let root_hash =
    let docv = "root_hash" in
    let doc = "The root hash is updated in consensus contract" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let open Term in
  const assert_root_hash $ folder_node $ root_hash

let _ = Cmd.eval @@ Cmd.v (Cmd.info "asserter") args
