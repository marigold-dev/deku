open Crypto;
open Protocol;

[@deriving yojson]
type snapshot = {
  hash: BLAKE2B.t,
  data: string,
};

type snapshot_ref;

type t =
  pri {
    current_snapshot: snapshot_ref,
    next_snapshots: list((int64, snapshot_ref)),
    last_block: Block.t,
    last_block_signatures: Signatures.t,
    additional_blocks: list(Block.t),
  };

let make:
  (
    ~initial_snapshot: snapshot,
    ~initial_block: Block.t,
    ~initial_signatures: Signatures.t
  ) =>
  t;

/** this should be called when a block is received */
let append_block: (~pool: Block_pool.t, (Block.t, Signatures.t), t) => t;
// TODO: add_snapshot_ref and start_new_epoch may be able to be fused into a single API since they should always happen together.
let add_snapshot_ref: (~block_height: int64, t) => (snapshot_ref, t);

let set_snapshot_ref: (snapshot_ref, snapshot) => unit;

let start_new_epoch: t => t;

let get_current_snapshot: t => option(snapshot);

let get_next_snapshot: t => option(snapshot);
