open Helpers;
open Protocol;

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

[@deriving yojson]
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
  last_snapshot: option(string),
  additional_blocks: list(Block.t),
};

let make = (~identity) => {
  identity,
  pending_side_ops: [],
  pending_main_ops: [],
  block_pool: Block_pool.make(~self_key=identity.t),
  applied_blocks: String_map.empty,
  applied_blocks_by_height: Int64_map.empty,
  last_applied_block_timestamp: 0.0,
  protocol: Protocol.empty,
  last_snapshot: None,
  additional_blocks: [],
};

let append_applied_block = (state, block) => {
  ...state,
  applied_blocks:
    state.applied_blocks |> String_map.add(block.Block.hash, block),
  applied_blocks_by_height:
    state.applied_blocks_by_height
    |> Int64_map.add(block.Block.block_height, block),
  additional_blocks: [block, ...state.additional_blocks],
};
let append_snapshot = state => {
  let protocol = state.protocol;
  let snapshot =
    // TODO: this clearly is dumb and slow;
    SHA256.hash(protocol)
    |> SHA256.to_yojson(Protocol.to_yojson)
    |> Yojson.Safe.pretty_to_string;
  // TODO: also sign the hash and broadcast, need to collect proofs of the snapshot
  {...state, last_snapshot: Some(snapshot), additional_blocks: []};
};
let apply_block = (state, block) => {
  let.ok protocol = apply_block(state.protocol, block);
  let state = {...state, protocol};
  let state = append_applied_block(state, block);
  // TODO: magic number
  let state =
    Int64.rem(protocol.block_height, 600L) == 0L
      ? append_snapshot(state) : state;

  Ok(state);
};
