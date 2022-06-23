open Helpers
open Crypto
open Protocol
open State
open Consensus_utils
open Apply_block

let load_snapshot ~snapshot ~additional_blocks ~last_block
    ~last_block_signatures t =
  let all_blocks =
    last_block :: additional_blocks
    |> List.sort (fun a b ->
           let open Int64 in
           to_int (sub a.Block.block_height b.Block.block_height)) in
  let block_pool =
    let block_pool =
      List.fold_left
        (fun block_pool block -> Block_pool.append_block block block_pool)
        t.block_pool all_blocks in
    let signatures_required = signatures_required t in
    List.fold_left
      (fun block_pool signature ->
        Block_pool.append_signature ~signatures_required
          ~hash:last_block.Block.hash signature block_pool)
      block_pool last_block_signatures in
  let%assert () =
    ( `Not_all_blocks_are_signed,
      List.for_all
        (fun block -> Block_pool.is_signed ~hash:block.Block.hash block_pool)
        all_blocks ) in
  let%assert () =
    ( `State_root_not_the_expected,
      snapshot.Snapshots.hash = last_block.state_root_hash ) in
  let%assert () =
    ( `Snapshots_with_invalid_hash,
      BLAKE2B.verify ~hash:snapshot.hash snapshot.data ) in
  let of_yojson =
    [%of_yojson:
      Core_deku.State.t
      * Tezos_operation_set.t
      * User_operation_set.t
      * Validators.t
      * BLAKE2B.t
      * int64
      * BLAKE2B.t
      * BLAKE2B.t] in
  let ( core_state,
        included_tezos_operations,
        included_user_operations,
        validators,
        validators_hash,
        block_height,
        last_block_hash,
        state_root_hash ) =
    snapshot.data |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok in
  let protocol =
    let open Protocol in
    {
      core_state;
      included_tezos_operations;
      included_user_operations;
      validators;
      validators_hash;
      block_height;
      last_block_hash;
      state_root_hash;
      last_state_root_update = 0.0;
      last_applied_block_timestamp = 0.0;
      last_seen_membership_change_timestamp = 0.0;
    } in
  (* TODO: this causes syncing to fail unless the snapshot is newer than the
     current block height, which is not always true (i.e, if you restart a node
     that's in sync very quickly. We need to have a more discerning validation
     here. *)
  let%assert () =
    (`Invalid_snapshot_height, protocol.block_height > t.protocol.block_height)
  in
  let t = { t with protocol; block_pool } in
  (* TODO: it doesn't seem valid to add a snapshot here based on the information
     received from our (untrusted) peer; however, that's exactly what we're
     doing here. Supposing the peer sending the snapshot is dishonest, we will
     then start propogating a bad snapshot. In the future, we should only add
     snapshots once we're in sync. *)
  List.fold_left_ok
    (fun prev_state block ->
      let%ok state, _step = apply_block ~block prev_state in
      if
        BLAKE2B.equal prev_state.protocol.state_root_hash
          state.protocol.state_root_hash
      then
        Ok state
      else
        (* TODO: this should be done in parallel, as otherwise the node may not
           be able to load the snapshot fast enough. *)
        let hash, data = Protocol.hash prev_state.protocol in
        let snapshot_ref, snapshots =
          Snapshots.add_snapshot_ref
            ~block_height:prev_state.protocol.block_height state.snapshots in
        let () = Snapshots.set_snapshot_ref snapshot_ref { hash; data } in
        Ok { state with snapshots })
    t all_blocks
