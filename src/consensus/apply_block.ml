open Helpers
open Crypto
open Protocol
open State
open Steps

let apply_block ~block state =
  let previous_protocol = state.protocol in
  (* TODO: handle this errors here *)
  let%ok protocol, user_operations, receipts =
    apply_block state.protocol block in
  let snapshots =
    if BLAKE2B.equal block.state_root_hash previous_protocol.state_root_hash
    then
      state.snapshots
    else
      Snapshots.start_new_epoch state.snapshots in

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

  Ok
    ( state,
      Both
        ( Effect
            (Applied_block
               { block; receipts; trusted_validator_membership_change }),
          Can_produce_block ) )
