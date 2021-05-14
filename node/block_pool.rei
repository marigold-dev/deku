open Protocol;

[@deriving yojson]
type block_and_signatures =
  pri {
    signatures: Signatures.t,
    block: option(Block.t),
    hash: string,
  };

[@deriving yojson]
type t;

let make: (~self_key: Address.t) => t;
let append_block: (Block.t, t) => t;
let append_signature:
  (~signatures_required: int, ~hash: string, Multisig.signature, t) => t;

let is_signed: (~hash: string, t) => bool;
let find_block: (~hash: string, t) => option(Block.t);
let find_signatures: (~hash: string, t) => option(Signatures.t);
let find_next_block_to_apply: (~hash: string, t) => option(Block.t);
let find_all_signed_blocks_above:
  ((Block.t, Signatures.t), t) => (list(Block.t), (Block.t, Signatures.t));

