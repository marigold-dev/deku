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
  let initial_snapshots = {
    let initial_snapshot = Protocol.hash(initial_protocol);
    Snapshots.make(~initial_snapshot, ~initial_block, ~initial_signatures);
  };
  {
    identity,
    pending_side_ops: [],
    pending_main_ops: [],
    block_pool: initial_block_pool,
    last_applied_block_timestamp: 0.0,
    protocol: initial_protocol,
    snapshots: initial_snapshots,
  };
};

let apply_block = (state, block) => {
  let.ok (protocol, new_snapshot) = apply_block(state.protocol, block);
  let state = {...state, protocol};
  switch (new_snapshot) {
  | Some(new_snapshot) =>
    let snapshots =
      Snapshots.update(
        ~new_snapshot,
        ~applied_block_height=state.protocol.block_height,
        state.snapshots,
      );
    Ok({...state, snapshots});
  | None => Ok(state)
  };
};
