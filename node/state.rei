open Helpers;
open Protocol;

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

module Address_map: Map.S with type key = Address.t;
module Uri_map: Map.S with type key = Uri.t;
module Int_map: Map.S with type key = int;

type t = {
  identity,
  trusted_validator_membership_change: Trusted_validators_membership_change.Set.t,
  interop_context: Tezos_interop.Context.t,
  data_folder: string,
  pending_side_ops: list(Operation.Side_chain.t),
  pending_main_ops: list(Operation.Main_chain.t),
  block_pool: Block_pool.t,
  protocol: Protocol.t,
  snapshots: Snapshots.t,
  current_epoch: int,
  finished_hashes: Int_map.t((BLAKE2B.t, string)),
  // networking
  uri_state: Uri_map.t(string),
  validators_uri: Address_map.t(Uri.t),
  recent_operation_results:
    BLAKE2B.Map.t(
      [
        | `Add_validator
        | `Remove_validator
        | `Transaction
        | `Withdraw(Ledger.Handle.t)
      ],
    ),
};

/** Adds a (hash, data) pair to the map of finished hashes,
    indexed by epoch. */
let add_finished_hash: (int, (BLAKE2B.t, string), t) => t;

/** Gets the hash corresponding to the [current_epoch]
    Returns [None] if it is not known. */
let get_next_hash: t => option((BLAKE2B.t, string));

let make:
  (
    ~identity: identity,
    ~trusted_validator_membership_change: Trusted_validators_membership_change.Set.t,
    ~interop_context: Tezos_interop.Context.t,
    ~data_folder: string,
    ~initial_validators_uri: Address_map.t(Uri.t)
  ) =>
  t;
let apply_block:
  (t, Block.t) =>
  result(t, [> | `Invalid_block_when_applying | `Invalid_state_root_hash]);

let load_snapshot:
  (
    ~state_root_hash: BLAKE2B.t,
    ~state_root: string,
    ~additional_blocks: list(Block.t),
    ~last_block: Block.t,
    ~last_block_signatures: list(Signature.t),
    t
  ) =>
  result(
    t,
    [>
      | `Invalid_block_when_applying
      | `Invalid_state_root_hash
      | `Not_all_blocks_are_signed
      | `Snapshots_with_invalid_hash
      | `State_root_not_the_expected
    ],
  );
