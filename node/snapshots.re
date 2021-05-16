open Helpers;
open Protocol;

/*
 last_block should always have the same or
 bigger block_height than the state snapshot,
 this ensures that if last_block is signed the
 state snapshot will also be signed */
type t = {
  last_snapshot: SHA256.t(string),
  last_block: Block.t,
  last_block_signatures: Signatures.t,
  additional_blocks: list(Block.t),
};

let make = (~initial_snapshot, ~initial_block, ~initial_signatures) => {
  last_snapshot: initial_snapshot,
  last_block: initial_block,
  last_block_signatures: initial_signatures,
  additional_blocks: [],
};

let append_block = (~pool, (block, signatures), t) =>
  // TODO: should check if there is additional signatures
  if (t.last_block.block_height > block.Block.block_height) {
    t;
  } else {
    let (blocks, (block, signatures)) =
      Block_pool.find_all_signed_blocks_above((block, signatures), pool);
    {
      last_snapshot: t.last_snapshot,
      last_block: block,
      last_block_signatures: signatures,
      additional_blocks: blocks @ t.additional_blocks,
    };
  };
let update = (~new_snapshot, ~applied_block_height, t) => {
  let rec truncate_additional_blocks = (block_height, blocks) =>
    switch (blocks) {
    | [hd, ...tl] when hd.Block.block_height > block_height => [
        hd,
        ...truncate_additional_blocks(block_height, tl),
      ]
    | _ => []
    };

  let additional_blocks =
    truncate_additional_blocks(applied_block_height, t.additional_blocks);
  {
    last_snapshot: new_snapshot,
    last_block: t.last_block,
    last_block_signatures: t.last_block_signatures,
    additional_blocks,
  };
};
