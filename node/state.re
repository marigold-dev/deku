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
  signatures: list(Multisig.signature),
  block: option(Block.t),
};

[@deriving yojson]
type t = {
  identity,
  pending_side_ops: list(Operation.Side_chain.Self_signed.t),
  pending_main_ops: list(Operation.Main_chain.t),
  pending_blocks: String_map.t(block_and_signatures),
  applied_blocks: String_map.t(Block.t),
  protocol: Protocol.t,
  last_applied_block_timestamp: float,
};

let make = (~identity) => {
  identity,
  pending_side_ops: [],
  pending_main_ops: [],
  pending_blocks: String_map.empty,
  applied_blocks: String_map.empty,
  last_applied_block_timestamp: 0.0,
  protocol: Protocol.empty,
};
