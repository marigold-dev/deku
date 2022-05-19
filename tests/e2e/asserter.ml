open Cmdliner
open Bin_common
open Crypto

let assert_state_correct data_folder expected_state_root_hash
    expected_block_height =
  print_endline @@ "Checking state at path: " ^ data_folder;
  let file = data_folder ^ "/state.bin" in
  let protocol_state = Lwt_main.run @@ Files.State_bin.read ~file in
  Format.printf "Minimum expected block height: %Ld. Actual height: %Ld\n%!"
    expected_block_height protocol_state.block_height;
  assert (protocol_state.block_height >= expected_block_height);
  let actual_state_root_hash =
    protocol_state.state_root_hash |> BLAKE2B.to_string in
  Format.printf "Expected state root hash: %s. Actual state root hash: %s\n%!"
    expected_state_root_hash actual_state_root_hash;
  assert (actual_state_root_hash = expected_state_root_hash);
  print_endline "State looks good 👍"
(* TODO: add more assertions *)

let args =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let expected_state_root_hash =
    let docv = "expected_state_root_hash" in
    let doc = "The expected state root hash of the node" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let expected_block_height =
    let docv = "exepcted_block_height" in
    let doc = "The expected block height of the node" in
    let open Arg in
    required & pos 2 (some int64) None & info [] ~doc ~docv in

  let open Term in
  const assert_state_correct
  $ folder_node
  $ expected_state_root_hash
  $ expected_block_height

let () = Term.exit @@ Term.eval (args, Term.info "asserter")
