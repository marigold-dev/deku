open Helpers;
open Protocol;

[@deriving yojson]
type block_and_signatures =
  pri {
    signatures: Signatures.t,
    block: option(Block.t),
    hash: string,
  };

[@deriving yojson]
type t =
  pri {
    self_key: Address.t,
    available: String_map.t(block_and_signatures),
    available_by_previous: String_map.t(block_and_signatures),
    signed: String_map.t(block_and_signatures),
    // TODO: is it possible to have two signed blocks at same level?
    signed_by_previous: String_map.t(block_and_signatures),
    last_signed_block: option(Block.t),
  };

let make: (~self_key: Address.t) => t;
let append_block: (Block.t, t) => t;
let append_signature:
  (~signatures_required: int, ~hash: string, Multisig.signature, t) => t;
