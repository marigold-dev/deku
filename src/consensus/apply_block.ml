open Helpers
open Crypto
open Protocol
open State
open Steps

let apply_block_header ~block state =
  let previous_protocol = state.protocol in
  let%ok protocol = apply_block_header state.protocol block in
  let state = { state with protocol } in
  Ok
    ( state,
      Both (Can_produce_block, Apply_block_data { block; previous_protocol }) )

let apply_block_data ~previous_protocol ~block state =
  (* TODO: handle this errors here *)
  let protocol, user_operations, receipts =
    apply_block_data state.protocol block in
  Metrics.Blocks.inc_operations_processed
    ~operation_count:(List.length user_operations);
  let snapshots, snapshot_ref =
    if BLAKE2B.equal block.state_root_hash previous_protocol.state_root_hash
    then
      (state.snapshots, None)
    else
      let snapshots = Snapshots.start_new_epoch state.snapshots in
      let snapshot_ref, snapshots =
        Snapshots.add_snapshot_ref ~block_height:previous_protocol.block_height
          snapshots in
      (snapshots, Some snapshot_ref) in

  (* TODO: definitely should separate on the pending *)
  let pending_operations =
    let remove_operations operations pending_operations =
      List.fold_left
        (fun pending_operations operation ->
          Operation_map.remove operation pending_operations)
        pending_operations operations in
    let consensus_operations =
      List.map
        (fun consensus_operation ->
          Protocol.Operation.Consensus consensus_operation)
        block.Block.consensus_operations in
    let tezos_operations =
      List.map
        (fun consensus_operation ->
          Protocol.Operation.Core_tezos consensus_operation)
        block.Block.tezos_operations in
    let user_operations =
      List.map
        (fun consensus_operation ->
          Protocol.Operation.Core_user consensus_operation)
        user_operations in
    state.State.pending_operations
    |> remove_operations consensus_operations
    |> remove_operations tezos_operations
    |> remove_operations user_operations in

  let trusted_validator_membership_change =
    List.fold_left
      (fun trusted_validator_membership_change operation ->
        match operation with
        | Protocol.Operation.Consensus.Add_validator validator ->
          Trusted_validators_membership_change.Set.remove
            { address = validator.address; action = Add }
            trusted_validator_membership_change
        | Remove_validator validator ->
          Trusted_validators_membership_change.Set.remove
            { address = validator.address; action = Remove }
            state.trusted_validator_membership_change)
      state.trusted_validator_membership_change block.consensus_operations in

  let state =
    {
      state with
      protocol;
      snapshots;
      pending_operations;
      trusted_validator_membership_change;
    } in

  let self_signed =
    match Block_pool.find_signatures ~hash:block.hash state.block_pool with
    | Some signatures ->
      if Signatures.is_self_signed signatures then Some signatures else None
    | None -> None in

  (* this is all the node is gonna see *)
  let applied_block_effect =
    Effect.Applied_block
      {
        prev_protocol = previous_protocol;
        block;
        receipts;
        self_signed;
        snapshot_ref;
      } in
  let persist_trusted_membership_change_effect =
    Effect.Persist_trusted_membership_change
      { trusted_validator_membership_change } in
  let effects =
    Both
      ( Effect applied_block_effect,
        Effect persist_trusted_membership_change_effect ) in
  Ok (state, snapshot_ref, Both (Post_apply_block, effects))
