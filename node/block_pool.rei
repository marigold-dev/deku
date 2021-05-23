open Helpers;
open Protocol;

type block_and_signatures =
  pri {
    signatures: Signatures.t,
    block: option(Block.t),
    hash: SHA256.hash,
  };

type t;

let make: (~self_key: Address.t) => t;
let append_block: (Block.t, t) => t;
let append_signature:
  (~signatures_required: int, ~hash: SHA256.hash, Signature.t, t) => t;

let is_signed: (~hash: SHA256.hash, t) => bool;
let find_block: (~hash: SHA256.hash, t) => option(Block.t);
let find_signatures: (~hash: SHA256.hash, t) => option(Signatures.t);
let find_next_block_to_apply: (~hash: SHA256.hash, t) => option(Block.t);
let find_all_signed_blocks_above:
  ((Block.t, Signatures.t), t) => (list(Block.t), (Block.t, Signatures.t));

