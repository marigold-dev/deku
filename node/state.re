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

let make = (~identity) => {
  let initial_block = Block.genesis;
  let initial_protocol = Protocol.make(~initial_block);
  let initial_signatures = Signatures.make(~self_key=identity.t);
  Signatures.set_signed(initial_signatures);

  let initial_block_pool =
    Block_pool.make(~self_key=identity.t)
    |> Block_pool.append_block(initial_block);
  let initial_snapshots =
    Snapshots.make(~initial_protocol, ~initial_block, ~initial_signatures);
  {
    identity,
    pending_side_ops: [],
    pending_main_ops: [],
    block_pool: initial_block_pool,
    applied_blocks: String_map.empty,
    applied_blocks_by_height: Int64_map.empty,
    last_applied_block_timestamp: 0.0,
    protocol: initial_protocol,
    snapshots: initial_snapshots,
  };
};

let apply_block = (state, block) => {
  let.ok protocol = apply_block(state.protocol, block);
  Ok({...state, protocol});
};
