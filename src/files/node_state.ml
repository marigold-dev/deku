open Helpers
open Protocol
open Node
open Consensus

(* FIXME: refactor below function to use this one *)
let get_initial_consensus_state ~folder:_ =
  failwith "Not implemented! Remove this when we're reading the right .bin"
  (*
  let%await identity =
    Config_files.Identity.read ~file:(folder ^ "/identity.json") in
  let trusted_validator_membership_change_file =
    folder ^ "/trusted-validator-membership-change.json" in
  let%await trusted_validator_membership_change_list =
    Config_files.Trusted_validators_membership_change.read
      ~file:trusted_validator_membership_change_file in
  let trusted_validator_membership_change =
    Trusted_validators_membership_change.Set.of_list
      trusted_validator_membership_change_list in
  let state_bin = folder ^ "/state.bin" in
  let%await state_bin_exists = Lwt_unix.file_exists state_bin in
  if state_bin_exists then
    Config_files.State_bin.read ~file:state_bin
  else
    await @@ Consensus.make ~identity ~trusted_validator_membership_change *)

let get_initial_state ~folder ~minimum_block_delay =
  let%await identity =
    Config_files.Identity.read ~file:(folder ^ "/identity.json") in
  let trusted_validator_membership_change_file =
    folder ^ "/trusted-validator-membership-change.json" in
  let%await trusted_validator_membership_change_list =
    Config_files.Trusted_validators_membership_change.read
      ~file:trusted_validator_membership_change_file in
  let trusted_validator_membership_change =
    Trusted_validators_membership_change.Set.of_list
      trusted_validator_membership_change_list in
  let%await interop_context =
    let%await {
            rpc_node;
            secret;
            consensus_contract;
            discovery_contract;
            required_confirmations;
          } =
      Config_files.Interop_context.read ~file:(folder ^ "/tezos.json") in
    Lwt.return
      (Tezos_interop.make ~rpc_node ~secret ~consensus_contract
         ~discovery_contract ~required_confirmations) in
  let%await validator_res =
    Tezos_interop.Consensus.fetch_validators interop_context in
  let validators =
    match validator_res with
    | Ok current_validators -> current_validators
    | Error err -> failwith err in
  let initial_validators_uri =
    List.fold_left
      (fun validators_uri (address, uri) ->
        match uri with
        | Some uri -> State.Address_map.add address uri validators_uri
        | None -> validators_uri)
      State.Address_map.empty validators in
  let persist_trusted_membership_change =
    Config_files.Trusted_validators_membership_change.write
      ~file:trusted_validator_membership_change_file in
  let config = Config.make ~identity ~minimum_block_delay in
  let node =
    State.make ~config ~trusted_validator_membership_change ~interop_context
      ~data_folder:folder ~initial_validators_uri
      ~persist_trusted_membership_change in

  let node =
    let validators =
      List.fold_left
        (fun validators (address, _) -> Validators.add { address } validators)
        Validators.empty validators in
    let protocol = { node.consensus.protocol with validators } in
    { node with consensus = { node.consensus with protocol } } in
  let state_bin = folder ^ "/state.bin" in
  let%await state_bin_exists = Lwt_unix.file_exists state_bin in
  let%await protocol =
    if state_bin_exists then
      Config_files.State_bin.read ~file:state_bin
    else
      await node.consensus.protocol in
  let prev_epoch_state_bin = folder ^ "/prev_epoch_state.bin" in
  let%await prev_epoch_state_bin_exists =
    Lwt_unix.file_exists prev_epoch_state_bin in
  let%await snapshots =
    if state_bin_exists && prev_epoch_state_bin_exists then
      let%await prev_protocol =
        Config_files.State_bin.read ~file:prev_epoch_state_bin in
      let hash, data = Protocol.hash prev_protocol in
      let snapshot_ref, snapshots =
        Snapshots.add_snapshot_ref ~block_height:prev_protocol.block_height
          node.consensus.snapshots in
      let () = Snapshots.set_snapshot_ref snapshot_ref { hash; data } in
      await (Snapshots.start_new_epoch snapshots)
    else
      await node.consensus.snapshots in
  let consensus = { node.consensus with protocol; snapshots } in
  let node = { node with consensus } in
  await node
