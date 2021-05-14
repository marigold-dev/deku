open Protocol;

type snapshot =
  pri {
    state: string,
    hash: string,
  };

type t =
  pri {
    last_snapshot: snapshot,
    last_block: Block.t,
    last_block_signatures: Signatures.t,
    additional_blocks: list(Block.t),
  };

let make:
  (
    ~initial_protocol: Protocol.t,
    ~initial_block: Block.t,
    ~initial_signatures: Signatures.t
  ) =>
  t;

/** this should be called when a block is received */
let append_block: (~pool: Block_pool.t, (Block.t, Signatures.t), t) => t;
let update: (~protocol: Protocol.t, (Block.t, Signatures.t), t) => t;
