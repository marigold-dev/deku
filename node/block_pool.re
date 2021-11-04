open Helpers;
open Crypto;
open Protocol;

// TODO: THE FAMOUS HASH MAP, maybe SHA256_map?
module Hash_map =
  Map.Make_with_yojson({
    [@deriving (ord, yojson)]
    type t = BLAKE2B.t;
  });
type block_and_signatures = {
  signatures: Signatures.t,
  block: option(Block.t),
  hash: BLAKE2B.t,
};

// TODO: clean all blocks older than the current state root hash
type t = {
  self_key: Wallet.t,
  available: Hash_map.t(block_and_signatures),
  available_by_previous: Hash_map.t(block_and_signatures),
  signed: Hash_map.t(block_and_signatures),
  // TODO: is it possible to have two signed blocks at same level?
  signed_by_previous: Hash_map.t((Block.t, Signatures.t)),
};

let update_block_and_signatures = (block_and_signatures, t) => {
  let add_to_map = Hash_map.add(_, block_and_signatures);
  let hash = block_and_signatures.hash;
  let is_signed = Signatures.is_signed(block_and_signatures.signatures);

  {
    self_key: t.self_key,
    available: add_to_map(hash, t.available),
    available_by_previous:
      switch (block_and_signatures.block) {
      | Some(block) =>
        add_to_map(block.previous_hash, t.available_by_previous)
      | None => t.available_by_previous
      },
    signed: is_signed ? add_to_map(hash, t.signed) : t.signed,
    signed_by_previous:
      switch (is_signed, block_and_signatures.block) {
      | (true, Some(block)) =>
        Hash_map.add(
          block.previous_hash,
          (block, block_and_signatures.signatures),
          t.signed_by_previous,
        )
      | _ => t.signed_by_previous
      },
  };
};

let find_block_and_signature_or_return_empty = (~hash, t) =>
  switch (Hash_map.find_opt(hash, t.available)) {
  | Some(block_and_signatures) => block_and_signatures
  | None =>
    let signatures = Signatures.make(~self_key=t.self_key);
    {signatures, block: None, hash};
  };

// TODO: test this
let is_signed = block_and_signatures =>
  Signatures.is_signed(block_and_signatures.signatures);
let rec set_signed = (block_and_signatures, t) => {
  let signatures = Signatures.set_signed(block_and_signatures.signatures);
  let block_and_signatures = {...block_and_signatures, signatures};
  let t = update_block_and_signatures(block_and_signatures, t);
  ensure_previous_is_signed(block_and_signatures, t);
}
and ensure_previous_is_signed = (block_and_signatures, t) =>
  switch (block_and_signatures.block) {
  | Some(block) =>
    let previous_block_and_signatures =
      find_block_and_signature_or_return_empty(~hash=block.previous_hash, t);
    if (is_signed(previous_block_and_signatures)) {
      t;
    } else {
      set_signed(previous_block_and_signatures, t);
    };
  | None => t
  };
let make = (~self_key) => {
  self_key,
  available: Hash_map.empty,
  available_by_previous: Hash_map.empty,
  signed: Hash_map.empty,
  signed_by_previous: Hash_map.empty,
};

let append_block = (block, t) => {
  let block_and_signatures =
    find_block_and_signature_or_return_empty(~hash=block.Block.hash, t);
  let block_and_signatures = {...block_and_signatures, block: Some(block)};
  // TODO: this is not general
  let block_and_signatures = {
    ...block_and_signatures,
    signatures:
      if (block.hash == Block.genesis.hash) {
        Signatures.set_signed(block_and_signatures.signatures);
      } else {
        block_and_signatures.signatures;
      },
  };
  let t = update_block_and_signatures(block_and_signatures, t);
  // TODO: this doesn't work if you receive the blocks in a different order
  if (is_signed(block_and_signatures)) {
    ensure_previous_is_signed(block_and_signatures, t);
  } else {
    t;
  };
};
let append_signature = (~signatures_required, ~hash, signature, t) => {
  let block_and_signatures =
    find_block_and_signature_or_return_empty(~hash, t);
  let block_and_signatures = {
    ...block_and_signatures,
    signatures:
      Signatures.add(
        ~signatures_required,
        signature,
        block_and_signatures.signatures,
      ),
  };
  let t = update_block_and_signatures(block_and_signatures, t);
  if (is_signed(block_and_signatures)) {
    ensure_previous_is_signed(block_and_signatures, t);
  } else {
    t;
  };
};

let is_signed = (~hash, t) => Hash_map.mem(hash, t.signed);
let find_block = (~hash, t) => {
  let.some {block, _} = Hash_map.find_opt(hash, t.available);
  block;
};
let find_signatures = (~hash, t) => {
  let.some {signatures, _} = Hash_map.find_opt(hash, t.available);
  Some(signatures);
};
// TODO: bad naming, means like, block_height + 1
let find_next_block_to_apply = (~hash, t) => {
  let.some (block, _) = Hash_map.find_opt(hash, t.signed_by_previous);
  Some(block);
};
let rec find_all_signed_blocks_above = (blocks, (block, signatures), t) => {
  // TODO: maybe just keep a list of signed_blocks sorted by the height
  switch (Hash_map.find_opt(block.Block.hash, t.signed_by_previous)) {
  | Some((another_block, signatures)) =>
    find_all_signed_blocks_above(
      [block, ...blocks],
      (another_block, signatures),
      t,
    )
  | None => (blocks, (block, signatures))
  };
};
let find_all_signed_blocks_above = find_all_signed_blocks_above([]);
