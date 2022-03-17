open Helpers
open Protocol
open Node
let get_initial_state ~folder =
  let%await identity = Files.Identity.read ~file:(folder ^ "/identity.json") in
  let trusted_validator_membership_change_file =
    folder ^ "/trusted-validator-membership-change.json" in
  let%await trusted_validator_membership_change_list =
    Files.Trusted_validators_membership_change.read
      ~file:trusted_validator_membership_change_file in
  let trusted_validator_membership_change =
    Trusted_validators_membership_change.Set.of_list
      trusted_validator_membership_change_list in
  let%await interop_context =
    let%await { rpc_node; secret; consensus_contract; required_confirmations } =
      Files.Interop_context.read ~file:(folder ^ "/tezos.json") in
    Lwt.return
      (Tezos_interop.make ~rpc_node ~secret ~consensus_contract
         ~required_confirmations) in
  let%await validator_res =
    Tezos_interop.Consensus.fetch_validators interop_context in
  let validators =
    match validator_res with
    | Ok current_validators ->
      current_validators
      |> List.mapi (fun i validator ->
             ( validator,
               Printf.sprintf "http://localhost:444%d" i |> Uri.of_string ))
    | Error err -> failwith err in
  let initial_validators_uri =
    List.fold_left
      (fun validators_uri (address, uri) ->
        State.Address_map.add address uri validators_uri)
      State.Address_map.empty validators in
  let persist_trusted_membership_change =
    Files.Trusted_validators_membership_change.write
      ~file:trusted_validator_membership_change_file in
  let node =
    State.make ~identity ~trusted_validator_membership_change ~interop_context
      ~data_folder:folder ~initial_validators_uri
      ~persist_trusted_membership_change in
  let node =
    {
      node with
      protocol =
        {
          node.protocol with
          validators =
            List.fold_left
              (fun validators (address, _) ->
                Validators.add { address } validators)
              Validators.empty validators;
        };
    } in
  let state_bin = folder ^ "/state.bin" in
  let%await state_bin_exists = Lwt_unix.file_exists state_bin in
  let%await protocol =
    if state_bin_exists then
      Files.State_bin.read ~file:state_bin
    else
      await node.protocol in
  let prev_epoch_state_bin = folder ^ "/prev_epoch_state.bin" in
  let%await prev_epoch_state_bin_exists =
    Lwt_unix.file_exists prev_epoch_state_bin in
  let%await snapshots =
    if state_bin_exists && prev_epoch_state_bin_exists then
      let%await prev_protocol =
        Files.State_bin.read ~file:prev_epoch_state_bin in
      let hash, data = Protocol.hash prev_protocol in
      let snapshot_ref, snapshots =
        Snapshots.add_snapshot_ref ~block_height:prev_protocol.block_height
          node.snapshots in
      let () = Snapshots.set_snapshot_ref snapshot_ref { hash; data } in
      await (Snapshots.start_new_epoch snapshots)
    else
      await node.snapshots in
  let node = { node with snapshots; protocol } in
  await node
