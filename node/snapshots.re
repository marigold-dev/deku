open Crypto;
open Protocol;
open Helpers;

[@deriving yojson]
type snapshot = {
  hash: BLAKE2B.t,
  data: string,
};

type snapshot_ref = Atomic.t(option(snapshot));

/*
 last_block should always have the same or
 bigger block_height than the state snapshot,
 this ensures that if last_block is signed the
 state snapshot will also be signed */
type t = {
  current_snapshot: snapshot_ref,
  next_snapshots: list((int64, snapshot_ref)),
  last_block: Block.t,
  last_block_signatures: Signatures.t,
  additional_blocks: list(Block.t),
};

let make = (~initial_snapshot, ~initial_block, ~initial_signatures) => {
  {
    current_snapshot:
      Atomic.make(
        // TODO: if a snapshot is requested before first epoch starts, we will send meaningless data.
        // We need to have logic in the snapshot request handler such that we send a 503 error or something
        // instead of sending bad data.
        Some({hash: initial_block.Block.state_root_hash, data: ""}),
      ),
    next_snapshots: [
      (initial_block.block_height, Atomic.make(Some(initial_snapshot))),
    ],
    last_block: initial_block,
    last_block_signatures: initial_signatures,
    additional_blocks: [],
  };
};

let append_block = (~pool, (block, signatures), t) =>
  // TODO: should check if there is additional signatures
  if (t.last_block.block_height > block.Block.block_height) {
    t;
  } else {
    let (blocks, (block, signatures)) =
      Block_pool.find_all_signed_blocks_above((block, signatures), pool);
    {
      current_snapshot: t.current_snapshot,
      next_snapshots: t.next_snapshots,
      last_block: block,
      last_block_signatures: signatures,
      additional_blocks: blocks @ [t.last_block] @ t.additional_blocks,
    };
  };

let add_snapshot_ref = (~block_height, t) => {
  let atom = Atomic.make(None);
  (
    atom,
    {
      next_snapshots: t.next_snapshots @ [(block_height, atom)],
      current_snapshot: t.current_snapshot,
      last_block: t.last_block,
      last_block_signatures: t.last_block_signatures,
      additional_blocks: t.additional_blocks,
    },
  );
};

let set_snapshot_ref = (ref_, snapshot) => {
  Format.printf(
    "\x1b[36m New protocol snapshot hash: %s\x1b[m\n%!",
    snapshot.hash |> BLAKE2B.to_string,
  );
  Atomic.set(ref_, Some(snapshot));
};

let start_new_epoch = t => {
  let rec truncate_additional_blocks = (block_height, blocks) =>
    switch (blocks) {
    | [hd, ...tl] when hd.Block.block_height > block_height => [
        hd,
        ...truncate_additional_blocks(block_height, tl),
      ]
    | _ => []
    };

  switch (t.next_snapshots) {
  | [(height, snapshot), ...tl] =>
    let additional_blocks =
      truncate_additional_blocks(height, t.additional_blocks);
    {
      current_snapshot: snapshot,
      next_snapshots: tl,
      last_block: t.last_block,
      last_block_signatures: t.last_block_signatures,
      additional_blocks,
    };

  | [] => failwith("You must add a snapshot before you can start a new epoch")
  };
};

let get_current_snapshot = t => Atomic.get(t.current_snapshot);

let get_next_snapshot = t => {
  let.some (_, snapshot) = List.nth_opt(t.next_snapshots, 0);
  Atomic.get(snapshot);
};
