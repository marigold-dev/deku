open Cmdliner
open Bin_common
open Crypto

[@@@part 0]

(* Function checking the state root hash  nd expected block height are correct *)

let assert_state_correct data_folder expected_state_root_hash
    expected_block_height =
  print_endline @@ "Check state at path: " ^ data_folder;
  (* declare the location of the file store information of state root hash *)
  (* TODO: run the node and check the contents store in state.bin *)
  let file = data_folder ^ "/state.bin" in
  (* read state of this file
     /bin/files.ml
     module State_bin
     val read ~file:string -> Protocol.t Lwt.t
     val write ~file:string -> unit Lwt.t

     Lwt_main.run: 'a Lwt.t -> 'a

     Protocol.t is defined in protocol/protocol_state.ml

     type t =
     {
      core_state : Deku_core.State.t; (* state of deku *)
      included_tezos_operations : Tezos_operation_set.t; (* tezos operations *)
      included_user_operations : User_operation_set.t; (* user operations *)
      validators : Validators.t; (* information about validators *)
      validators_hash : BLAKE2B.t; (* validators hashes *)
      block_height : int64; (* block height, init is 1L *)
      last_block_hash : BLAKE2B.t; (* the last block hash: is the previous_hash *)
      state_root_hash : BLAKE2B.t; (* state root hash: is the state_root_hash  *)
      last_state_root_update : float; (* last state root update, init is 0.0 *)
      last_applied_block_timestamp : float; (* last applied block timestamp, init is 0.0 *)
      last_seen_membership_change_timestamp : float; (* last seen membership change timestamp, init is 0.0 *)
     }
  *)
  let protocol_state : Protocol.t = Lwt_main.run @@ Files.State_bin.read ~file in
  (* Print the information to check *)
  Format.printf "Minimum expected block height: %Ld. Actual height: %Ld\n!"
    expected_block_height protocol_state.block_height;
  (* get the state root hash of the protocol state *)
  let actual_state_root_hash =
    protocol_state.state_root_hash |> BLAKE2B.to_string in
  (* print this state root hash out *)
  Format.printf "Expected state root hash: %s. Actual state root hash: %s\n!"
    expected_state_root_hash actual_state_root_hash;
  (* Then check the state root hash is the expected one *)
  assert (actual_state_root_hash = expected_state_root_hash);
  (* finish*)
  print_endline "The state root hash is correct"

(*
  These args will show at command line 
*)

[@@@part "1"]

let args =
  (* Define folder that stores node data *)
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  (* argument of the expected state root hash *)
  (* TODO: this is the hash that I need to benchmark *)
  let expected_state_root_hash =
    let docv = "expected_state_root_hash" in
    let doc = "The expected state root hash of the node" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let expected_block_height =
    let docv = "expected_block_height" in
    let doc = "The expected block height of the node" in
    let open Arg in
    required & pos 2 (some int64) None & info [] ~doc ~docv in
  let open Term in
  const assert_state_correct
  $ folder_node
  $ expected_state_root_hash
  $ expected_block_height

let main = Term.exit @@ Term.eval (args, Term.info "asserter")
