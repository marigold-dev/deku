open Helpers;
open Protocol;

type t =
  pri {
    last_snapshot: SHA256.t(string),
    last_block: Block.t,
    last_block_signatures: Signatures.t,
    additional_blocks: list(Block.t),
  };

let make:
  (
    ~initial_snapshot: SHA256.t(string),
    ~initial_block: Block.t,
    ~initial_signatures: Signatures.t
  ) =>
  t;

/** this should be called when a block is received */
let append_block: (~pool: Block_pool.t, (Block.t, Signatures.t), t) => t;
let update:
  (~new_snapshot: SHA256.t(string), ~applied_block_height: int64, t) => t;
