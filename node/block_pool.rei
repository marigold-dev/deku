open Crypto;
open Protocol;

type block_and_signatures =
  pri {
    signatures: Signatures.t,
    block: option(Block.t),
    hash: BLAKE2B.t,
  };

type t;

let make: (~self_key: Wallet.t) => t;
let append_block: (Block.t, t) => t;
let append_signature:
  (~signatures_required: int, ~hash: BLAKE2B.t, Signature.t, t) => t;

let is_signed: (~hash: BLAKE2B.t, t) => bool;
let find_block: (~hash: BLAKE2B.t, t) => option(Block.t);
let find_signatures: (~hash: BLAKE2B.t, t) => option(Signatures.t);
let find_next_block_to_apply: (~hash: BLAKE2B.t, t) => option(Block.t);
let find_all_signed_blocks_above:
  ((Block.t, Signatures.t), t) => (list(Block.t), (Block.t, Signatures.t));
