open Helpers;
open Protocol;

[@deriving yojson]
type block_and_signatures = {
  signatures: Signatures.t,
  block: option(Block.t),
  hash: string,
};

[@deriving yojson]
type t = {
  self_key: Address.t,
  available: String_map.t(block_and_signatures),
  available_by_previous: String_map.t(block_and_signatures),
  signed: String_map.t(block_and_signatures),
  // TODO: is it possible to have two signed blocks at same level?
  signed_by_previous: String_map.t(block_and_signatures),
  last_signed_block: option(Block.t),
};

let update_block_and_signatures = (block_and_signatures, t) => {
  let add_to_map = String_map.add(_, block_and_signatures);
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
        add_to_map(block.previous_hash, t.signed_by_previous)
      | _ => t.signed_by_previous
      },
    last_signed_block:
      switch (t.last_signed_block, block_and_signatures.block) {
      | (Some(old_block), Some(block))
          when block.block_height > old_block.block_height =>
        Some(block)
      | (None, Some(block)) => Some(block)
      | (_, None)
      | (Some(_), Some(_)) => t.last_signed_block
      },
  };
};

let find_block_and_signature_or_return_empty = (~hash, t) =>
  switch (String_map.find_opt(hash, t.available)) {
  | Some(block_and_signatures) => block_and_signatures
  | None =>
    let signatures = Signatures.make(~self_key=t.self_key);
    {signatures, block: None, hash};
  };

// TODO: test this
let is_signed = block_and_signatures =>
  Signatures.is_signed(block_and_signatures.signatures);
let rec set_signed = (block_and_signatures, t) => {
  Signatures.set_signed(block_and_signatures.signatures);
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
  available: String_map.empty,
  available_by_previous: String_map.empty,
  signed: String_map.empty,
  signed_by_previous: String_map.empty,
  last_signed_block: None,
};
let append_block = (block, t) => {
  let block_and_signatures =
    find_block_and_signature_or_return_empty(~hash=block.Block.hash, t);
  let block_and_signatures = {...block_and_signatures, block: Some(block)};
  let t = update_block_and_signatures(block_and_signatures, t);
  if (is_signed(block_and_signatures)) {
    ensure_previous_is_signed(block_and_signatures, t);
  } else {
    t;
  };
};
let append_signature = (~signatures_required, ~hash, signature, t) => {
  let block_and_signatures =
    find_block_and_signature_or_return_empty(~hash, t);
  Signatures.add(
    ~signatures_required,
    signature,
    block_and_signatures.signatures,
  );
  let t = update_block_and_signatures(block_and_signatures, t);
  if (is_signed(block_and_signatures)) {
    ensure_previous_is_signed(block_and_signatures, t);
  } else {
    t;
  };
};
