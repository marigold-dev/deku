open Helpers;
open Protocol;

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

type t = {
  identity,
  pending_side_ops: list(Operation.Side_chain.Self_signed.t),
  pending_main_ops: list(Operation.Main_chain.t),
  block_pool: Block_pool.t,
  // TODO: make so that genesis happens through a different pipeline
  // TODO: then the CLI can inject block 2 instead, like if that was a stale
  applied_blocks: String_map.t(Block.t),
  applied_blocks_by_height: Int64_map.t(Block.t),
  protocol: Protocol.t,
  last_applied_block_timestamp: float,
  snapshots: Snapshots.t,
};

let make: (~identity: identity) => t;
let apply_block:
  (t, Block.t) =>
  result(t, [> | `Invalid_block_when_applying | `Invalid_state_root_hash]);
