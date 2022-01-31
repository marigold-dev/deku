open Helpers;
open Crypto;
open Protocol;
open Core;

[@deriving yojson]
type identity = {
  secret: Secret.t,
  key: Key.t,
  t: Address.t,
  uri: Uri.t,
};

module Address_map = Map.Make(Address);
module Uri_map = Map.Make(Uri);

type t = {
  identity,
  trusted_validator_membership_change: Trusted_validators_membership_change.Set.t,
  interop_context: Tezos_interop.Context.t,
  data_folder: string,
  pending_operations: list(Protocol.Operation.t),
  block_pool: Block_pool.t,
  protocol: Protocol.t,
  snapshots: Snapshots.t,
  // networking
  // TODO: move this to somewhere else but the string means the nonce needed
  // TODO: someone right now can spam the network to prevent uri changes
  // TODO: clean this once in a while
  // TODO: clean after nonce is used
  uri_state: Uri_map.t(string),
  validators_uri: Address_map.t(Uri.t),
  // TODO: use proper variants in the future
  // TODO: this also needs to be cleaned in the future
  recent_operation_receipts: BLAKE2B.Map.t(Core.State.receipt),
  persist_trusted_membership_change:
    list(Trusted_validators_membership_change.t) => Lwt.t(unit),
};

let make =
    (
      ~identity,
      ~trusted_validator_membership_change,
      ~persist_trusted_membership_change,
      ~interop_context,
      ~data_folder,
      ~initial_validators_uri,
    ) => {
  let initial_block = Block.genesis;
  let initial_protocol = Protocol.make(~initial_block);
  let initial_signatures =
    Signatures.make(~self_key=identity.key) |> Signatures.set_signed;

  let initial_block_pool =
    Block_pool.make(~self_key=identity.key)
    |> Block_pool.append_block(initial_block);
  let (hash, data) = Protocol.hash(initial_protocol);
  let initial_snapshot = Snapshots.{hash, data};
  let initial_snapshots =
    Snapshots.make(~initial_snapshot, ~initial_block, ~initial_signatures);

  {
    identity,
    trusted_validator_membership_change,
    interop_context,
    data_folder,
    pending_operations: [],
    block_pool: initial_block_pool,
    protocol: initial_protocol,
    snapshots: initial_snapshots,
    // networking
    uri_state: Uri_map.empty,
    validators_uri: initial_validators_uri,
    recent_operation_receipts: BLAKE2B.Map.empty,
    persist_trusted_membership_change,
  };
};

let apply_block = (state, block) => {
  let prev_protocol = state.protocol;
  let.ok (protocol, receipts) = Protocol.apply_block(state.protocol, block);
  let snapshots =
    if (Crypto.BLAKE2B.equal(
          block.state_root_hash,
          prev_protocol.state_root_hash,
        )) {
      state.snapshots;
    } else {
      Snapshots.start_new_epoch(state.snapshots);
    };
  let recent_operation_receipts =
    List.fold_left(
      (results, (hash, receipt)) => BLAKE2B.Map.add(hash, receipt, results),
      state.recent_operation_receipts,
      receipts,
    );
  Ok({...state, protocol, recent_operation_receipts, snapshots});
};

// TODO: duplicated code
let signatures_required = state => {
  let number_of_validators = Validators.length(state.protocol.validators);
  // TODO: properly filter and check signatures
  Float.(to_int(ceil(of_int(number_of_validators) *. (2.0 /. 3.0))));
};
let load_snapshot =
    (
      ~snapshot,
      ~additional_blocks,
      ~last_block,
      // TODO: this is bad, Signatures.t is a private type and not a network one
      ~last_block_signatures,
      t,
    ) => {
  let all_blocks =
    [last_block, ...additional_blocks]
    |> List.sort((a, b) =>
         Int64.(to_int(sub(a.Block.block_height, b.Block.block_height)))
       );
  let block_pool = {
    let block_pool =
      List.fold_left(
        (block_pool, block) => Block_pool.append_block(block, block_pool),
        t.block_pool,
        all_blocks,
      );
    let signatures_required = signatures_required(t);
    List.fold_left(
      (block_pool, signature) =>
        Block_pool.append_signature(
          ~signatures_required,
          ~hash=last_block.Block.hash,
          signature,
          block_pool,
        ),
      block_pool,
      last_block_signatures,
    );
  };
  let.assert () = (
    `Not_all_blocks_are_signed,
    List.for_all(
      block => Block_pool.is_signed(~hash=block.Block.hash, block_pool),
      all_blocks,
    ),
  );
  let.assert () = (
    `State_root_not_the_expected,
    // TODO: It may not hold in the future that the last block's
    // state root hash is equal to this state root's hash. E.g.,
    // we may send blocks from the requested epoch and future ones.
    // In the future, the equivalent check should be something like this:
    // let hd_srh == List.hd(all_blocks).state_root_hash;
    // state_root_hash == List.find(b => b.srh != hd_srh)
    snapshot.Snapshots.hash == last_block.state_root_hash,
  );
  let.assert () = (
    `Snapshots_with_invalid_hash,
    BLAKE2B.verify(~hash=snapshot.hash, snapshot.data),
  );
  let of_yojson = [%of_yojson:
    (
      Core.State.t,
      Tezos_operation_set.t,
      User_operation_set.t,
      Validators.t,
      BLAKE2B.t,
      int64,
      BLAKE2B.t,
      BLAKE2B.t,
    )
  ];
  let (
    core_state,
    included_tezos_operations,
    included_user_operations,
    validators,
    validators_hash,
    block_height,
    last_block_hash,
    state_root_hash,
  ) =
    // TODO: verify the hash
    snapshot.data |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok;

  // TODO: this is clearly an abstraction leak

  let protocol =
    Protocol.{
      core_state,
      included_tezos_operations,
      included_user_operations,
      validators,
      validators_hash,
      block_height,
      last_block_hash,
      state_root_hash,
      last_state_root_update: 0.0,
      last_applied_block_timestamp: 0.0,
      last_seen_membership_change_timestamp: 0.0,
    };
  // We should only ever load snapshots of the future, never the past.
  let.assert () = (
    `Invalid_snapshot_height,
    protocol.block_height > t.protocol.block_height,
  );
  let t = {...t, protocol, block_pool};
  // TODO: it doesn't seem valid to add a snapshot here based on the information received
  // from our (untrusted) peer; however, that's exactly what we're doing here. Supposing
  // the peer sending the snapshot is dishonest, we will then start propogating a bad snapshot.
  // In the future, we should only add snapshots once we're in sync.
  List.fold_left_ok(
    (prev_state, block) => {
      let.ok state = apply_block(prev_state, block);
      if (BLAKE2B.equal(
            prev_state.protocol.state_root_hash,
            state.protocol.state_root_hash,
          )) {
        Ok(state);
      } else {
        let (hash, data) = Protocol.hash(prev_state.protocol);
        let (snapshot_ref, snapshots) =
          Snapshots.add_snapshot_ref(
            ~block_height=prev_state.protocol.block_height,
            state.snapshots,
          );
        let () = Snapshots.set_snapshot_ref(snapshot_ref, {hash, data});
        Ok({...state, snapshots});
      };
    },
    t,
    all_blocks,
  );
};
