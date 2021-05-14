open Helpers;
open Protocol;

type snapshot = {
  state: string,
  // TODO: there is no good reason for this to exists
  hash: string,
};
let snapshot = protocol => {
  let hash = SHA256.hash(protocol).hash;
  let state = protocol |> Protocol.to_yojson |> Yojson.Safe.pretty_to_string;
  {state, hash};
};

/*
 last_block should always have the same or
 bigger block_height than the state snapshot,
 this ensures that if last_block is signed the
 state snapshot will also be signed */
type t = {
  last_snapshot: snapshot,
  last_block: Block.t,
  last_block_signatures: Signatures.t,
  additional_blocks: list(Block.t),
};

let make = (~initial_protocol, ~initial_block, ~initial_signatures) => {
  last_snapshot: snapshot(initial_protocol),
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
let update = (~protocol, (block, signatures), t) => {
  let rec truncate_additional_blocks = (block_height, blocks) =>
    switch (blocks) {
    | [hd, ...tl] when hd.Block.block_height > block_height => [
        hd,
        ...truncate_additional_blocks(block_height, tl),
      ]
    | _ => []
    };
  // TODO: we could hash the snapshot only if there is no pending hash on the list
  let last_snapshot = snapshot(protocol);

  let (block, signatures, additional_blocks) =
    if (block.Block.block_height > t.last_block.block_height) {
      (block, signatures, []);
    } else {
      let additional_blocks =
        truncate_additional_blocks(
          protocol.block_height,
          t.additional_blocks,
        );
      (t.last_block, t.last_block_signatures, additional_blocks);
    };
  {
    last_snapshot,
    last_block: block,
    last_block_signatures: signatures,
    additional_blocks,
  };
};
/*
 let load = (~hash, state, t) => {
   let.ok protocol = state |> Yojson.Safe.from_string |> Protocol.of_yojson;
   let.ok {hash,_} = SHA256.verify(~hash, protocol);

   Ok(protocol);
 };
 */
