open Helpers
open Protocol
open State
open Steps

(** Calculates whether to start sending a new state root hash.\n\n\ The state
    root epoch is the interval (in blocks) between state root\n\ hash updates.
    Thus, a new epoch is triggered by applying a block with\n\ a new state root
    hash. The block producer decides when to send\n\ blocks with new state root
    hashes. To enforce that he does so on time,\n\ validators reject blocks with
    updates that occur to soon or too late\n\ (see [Protocol.apply]).\n\n\ The
    block producer uses this function to determine when to send a\n\ block with
    an updated state root hash *)
let should_start_new_epoch last_state_root_update current_time =
  let avoid_jitter = 1.0 in
  current_time -. last_state_root_update -. avoid_jitter
  >= minimum_signable_time_between_epochs

(* TODO: this is an workaround solution, replace it by Tezos_rpc *)
let can_include_tezos_operation ~current_time ~requested_at =
  current_time -. requested_at > minimum_waiting_period_for_tezos_operation

let produce_block state =
  let current_time = Unix.time () in
  let start_new_epoch =
    should_start_new_epoch state.protocol.last_state_root_update current_time
  in
  let next_state_root_hash =
    if start_new_epoch then
      let%some snapshot = Snapshots.get_next_snapshot state.snapshots in
      Some snapshot.hash
    else
      None in
  let operations =
    (* TODO: fold into list on Helpers *)
    Operation_map.fold
      (fun operation requested_at operations ->
        match operation with
        | Operation.Core_tezos _ ->
          if can_include_tezos_operation ~current_time ~requested_at then
            operation :: operations
          else
            operations
        | Core_user _
        | Consensus _ ->
          operation :: operations)
      state.pending_operations [] in
  (* TODO: probably separate operations at pending_operations? *)
  let consensus_operations, tezos_operations, user_operations =
    List.fold_left
      (fun (consensus_operations, tezos_operations, user_operations) operation ->
        match operation with
        | Protocol.Operation.Consensus consensus_operation ->
          ( consensus_operation :: consensus_operations,
            tezos_operations,
            user_operations )
        | Core_tezos tezos_operation ->
          ( consensus_operations,
            tezos_operation :: tezos_operations,
            user_operations )
        | Core_user user_operation ->
          ( consensus_operations,
            tezos_operations,
            user_operation :: user_operations ))
      ([], [], []) operations in
  let block =
    Block.produce ~state:state.protocol ~author:state.identity.t
      ~next_state_root_hash ~consensus_operations ~tezos_operations
      ~user_operations in
  Both (Effect (Broadcast_block { block }), Check_block { block })
