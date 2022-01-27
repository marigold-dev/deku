open Crypto;
open Protocol;

[@deriving yojson]
type snapshot = {
  hash: BLAKE2B.t,
  data: string,
};

type t =
  pri {
    last_snapshot: snapshot,
    last_snapshot_height: int64,
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
let update: (~new_snapshot: snapshot, ~applied_block_height: int64, t) => t;
