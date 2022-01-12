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

module Address_map: Map.S with type key = Address.t;
module Uri_map: Map.S with type key = Uri.t;
module Int64_map: Map.S with type key = int64;

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
  next_state_root_hash_block_height: int64,
  finished_hashes: Int64_map.t((BLAKE2B.t, string)),
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
  persist_trusted_membership_change:
    list(Trusted_validators_membership_change.t) => Lwt.t(unit),
};

/** Adds a (hash, data) pair to the map of finished hashes,
    indexed by the block height it was generated on. */
let add_finished_hash: (int64, (BLAKE2B.t, string), t) => t;

/** Gets the next state root hash.
    Returns [None] if it is not known. */
let get_next_hash: t => option((BLAKE2B.t, string));

let make:
  (
    ~identity: identity,
    ~trusted_validator_membership_change: Trusted_validators_membership_change.Set.t,
    ~persist_trusted_membership_change: list(
                                          Trusted_validators_membership_change.t,
                                        ) =>
                                        Lwt.t(unit),
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
