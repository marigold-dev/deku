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
  uri_state: Uri_map.t(string),
  validators_uri: Address_map.t(Uri.t),
  recent_operation_receipts: BLAKE2B.Map.t(Core.State.receipt),
  persist_trusted_membership_change:
    list(Trusted_validators_membership_change.t) => Lwt.t(unit),
};

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
    ~snapshot: Snapshots.snapshot,
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
      | `Invalid_snapshot_height
    ],
  );
