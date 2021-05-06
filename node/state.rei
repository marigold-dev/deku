open Helpers;
open Protocol;

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

[@deriving yojson]
type block_and_signatures = {
  // TODO: this should probably be a set
  signatures: Signatures.t,
  block: option(Block.t),
  hash: string,
};

[@deriving yojson]
type t = {
  identity,
  pending_side_ops: list(Operation.Side_chain.Self_signed.t),
  pending_main_ops: list(Operation.Main_chain.t),
  pending_blocks: String_map.t(block_and_signatures),
  pending_blocks_by_previous: String_map.t(list(block_and_signatures)),
  // TODO: make so that genesis happens through a different pipeline
  // TODO: then the CLI can inject block 2 instead, like if that was a stale
  last_signed_block: option(Block.t),
  applied_blocks: String_map.t(Block.t),
  applied_blocks_by_height: Int64_map.t(Block.t),
  protocol: Protocol.t,
  last_applied_block_timestamp: float,
  last_snapshot: option(string),
  additional_blocks: list(Block.t),
};

let make: (~identity: identity) => t;
let apply_block:
  (t, Block.t) => result(t, [> | `Invalid_block_height_when_applying]);
