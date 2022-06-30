open Protocol
open State
open Consensus_utils
open Helpers

let is_signed_by_self state ~hash =
  match Block_pool.find_signatures ~hash state.block_pool with
  | Some signatures -> Signatures.is_self_signed signatures
  | None -> false

let has_next_block_to_apply state ~hash =
  match Block_pool.find_next_block_to_apply ~hash state.block_pool with
  | Some _ -> true
  | None -> false

let block_has_signable_state_root_hash ~current_time state block =
  let time_since_last_epoch =
    current_time -. state.protocol.last_state_root_update in
  if block_matches_current_state_root_hash state block then
    time_since_last_epoch <= maximum_signable_time_between_epochs
  else
    block_matches_next_state_root_hash state block
    && time_since_last_epoch >= minimum_signable_time_between_epochs

let is_signable state block =
  let { trusted_validator_membership_change; protocol; _ } = state in
  let current_time = Unix.time () in
  let next_allowed_membership_change_timestamp =
    protocol.last_seen_membership_change_timestamp +. (24. *. 60. *. 60.) in

  let is_trusted_consensus_operation consensus_operation =
    current_time > next_allowed_membership_change_timestamp
    &&
    match consensus_operation with
    | Protocol.Operation.Consensus.Add_validator validator ->
      Trusted_validators_membership_change.Set.mem
        { address = validator.address; action = Add }
        trusted_validator_membership_change
    | Remove_validator validator ->
      Trusted_validators_membership_change.Set.mem
        { address = validator.address; action = Remove }
        trusted_validator_membership_change in
  let is_trusted_tezos_operation _tezos_operation = true in

  (* TODO: this is a bit hackish *)
  (* let operation = Protocol.Operation.Core_tezos tezos_operation in
     Operation_map.mem operation state.pending_operations in *)
  let all_operations_are_trusted =
    List.for_all is_trusted_consensus_operation block.Block.consensus_operations
    && List.for_all is_trusted_tezos_operation block.Block.tezos_operations
  in
  let block_analysis =
    [
      (is_next state block, "Not next block");
      (not (is_signed_by_self state ~hash:block.hash), "Already signed by self");
      ( is_current_producer state ~key_hash:block.author,
        "Is not authored by current block producer" );
      ( not (has_next_block_to_apply state ~hash:block.hash),
        "Is already have next block to apply" );
      (all_operations_are_trusted, "Includes untrusted Tezos operations");
      ( block_has_signable_state_root_hash ~current_time state block,
        "Does not include a signable state root hash" );
    ] in
  let rejection_reasons =
    List.fold_left
      (fun acc (is_valid, failure_reason) ->
        if not is_valid then
          failure_reason :: acc
        else
          acc)
      [] block_analysis in
  if Int.equal (List.length rejection_reasons) 0 then
    true
  else (
    Log.error "Refusing to sign %a. Rejection reasons: %s" Block.pp block
      (String.concat "; " rejection_reasons);
    false)
