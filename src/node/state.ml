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
type t = {
  identity : identity;
  trusted_validator_membership_change :
    Trusted_validators_membership_change.Set.t;
  interop_context : Tezos_interop.Context.t;
  data_folder : string;
  pending_operations : Protocol.Operation.t list;
  block_pool : Block_pool.t;
  protocol : Protocol.t;
  snapshots : Snapshots.t;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core.State.receipt BLAKE2B.Map.t;
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
    pending_operations = [];
    block_pool = initial_block_pool;
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
      let hash, data = Protocol.hash prev_protocol in
      state.snapshots
      |> Snapshots.start_new_epoch
      |> Snapshots.add_snapshot
           ~new_snapshot:
             (let open Snapshots in
             { hash; data })
           ~block_height:prev_protocol.block_height in
  let recent_operation_receipts =
    List.fold_left
      (fun results (hash, receipt) -> BLAKE2B.Map.add hash receipt results)
      state.recent_operation_receipts receipts in
  Ok { state with protocol; recent_operation_receipts; snapshots }
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
      Core.State.t
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
  let%assert () =
    (`Invalid_snapshot_height, protocol.block_height > t.protocol.block_height)
  in
  let t = { t with protocol; block_pool } in
  List.fold_left_ok apply_block t all_blocks
