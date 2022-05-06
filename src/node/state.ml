open Helpers
open Crypto
open Protocol

type identity = {
  secret : Secret.t;
  key : Key.t;
  t : Key_hash.t;
  uri : Uri.t;
}
[@@deriving yojson]

module Address_map = Map.Make (Key_hash)
module Uri_map = Map.Make (Uri)
module Operation_map = Map.Make (Operation)

(* TODO: proper type for timestamps *)
type timestamp = float

type t = {
  identity : identity;
  trusted_validator_membership_change :
    Trusted_validators_membership_change.Set.t;
  interop_context : Tezos_interop.t;
  data_folder : string;
  pending_operations : timestamp Operation_map.t;
  block_pool : Block_pool.t;
  (* TODO: we need a bound on the size of this and put
     behind an abstract type. We should also change how
     this works once we have an indexer. See https://github.com/marigold-dev/deku/issues/535 *)
  applied_blocks : Block.t list;
  protocol : Protocol.t;
  snapshots : Snapshots.t;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core_deku.State.receipt BLAKE2B.Map.t;
  persist_trusted_membership_change :
    Trusted_validators_membership_change.t list -> unit Lwt.t;
}

let make ~identity ~trusted_validator_membership_change
    ~persist_trusted_membership_change ~interop_context ~data_folder
    ~initial_validators_uri =
  let initial_block = Block.genesis in
  let initial_protocol = Protocol.make ~initial_block in
  let initial_signatures =
    Signatures.make ~self_key:identity.key |> Signatures.set_signed in
  let initial_block_pool =
    Block_pool.make ~self_key:identity.key
    |> Block_pool.append_block initial_block in
  let hash, data = Protocol.hash initial_protocol in
  let initial_snapshot =
    let open Snapshots in
    { hash; data } in
  let initial_snapshots =
    Snapshots.make ~initial_snapshot ~initial_block ~initial_signatures in
  {
    identity;
    trusted_validator_membership_change;
    interop_context;
    data_folder;
    pending_operations = Operation_map.empty;
    block_pool = initial_block_pool;
    applied_blocks = [];
    protocol = initial_protocol;
    snapshots = initial_snapshots;
    uri_state = Uri_map.empty;
    validators_uri = initial_validators_uri;
    recent_operation_receipts = BLAKE2B.Map.empty;
    persist_trusted_membership_change;
  }

let apply_block state block =
  let prev_protocol = state.protocol in
  let%ok protocol, receipts = Protocol.apply_block state.protocol block in
  let snapshots =
    if Crypto.BLAKE2B.equal block.state_root_hash prev_protocol.state_root_hash
    then
      state.snapshots
    else
      Snapshots.start_new_epoch state.snapshots in
  let recent_operation_receipts =
    List.fold_left
      (fun results (hash, receipt) -> BLAKE2B.Map.add hash receipt results)
      state.recent_operation_receipts receipts in
  let applied_blocks = block :: state.applied_blocks in
  (* TODO: we might want to use a monotonic clock so we don't get weird things like
     negative block rates. *)
  let timestamp = Unix.time () in
  let operation_count = List.length block.operations in
  Metrics.Througput.collect_block_metrics ~timestamp ~operation_count;
  Ok
    {
      state with
      protocol;
      recent_operation_receipts;
      snapshots;
      applied_blocks;
    }

let signatures_required state =
  let number_of_validators = Validators.length state.protocol.validators in
  let open Float in
  to_int (ceil (of_int number_of_validators *. (2.0 /. 3.0)))

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
     current block height, which is not always true (i.e, if you restart
     a node that's in sync very quickly. We need to have a more discerning
     validation here. *)
  let%assert () =
    (`Invalid_snapshot_height, protocol.block_height > t.protocol.block_height)
  in
  let t = { t with protocol; block_pool } in
  (* TODO: it doesn't seem valid to add a snapshot here based on the information received
     from our (untrusted) peer; however, that's exactly what we're doing here. Supposing
     the peer sending the snapshot is dishonest, we will then start propogating a bad snapshot.
     In the future, we should only add snapshots once we're in sync. *)
  List.fold_left_ok
    (fun prev_state block ->
      let%ok state = apply_block prev_state block in
      if
        BLAKE2B.equal prev_state.protocol.state_root_hash
          state.protocol.state_root_hash
      then
        Ok state
      else
        (* TODO: this should be done in parallel, as otherwise the node
           may not be able to load the snapshot fast enough. *)
        let hash, data = Protocol.hash prev_state.protocol in
        let snapshot_ref, snapshots =
          Snapshots.add_snapshot_ref
            ~block_height:prev_state.protocol.block_height state.snapshots in
        let () = Snapshots.set_snapshot_ref snapshot_ref { hash; data } in
        Ok { state with snapshots })
    t all_blocks
