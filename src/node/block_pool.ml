open Helpers
open Crypto
open Protocol
module Hash_map = Map.Make_with_yojson (struct
  type t = BLAKE2B.t [@@deriving ord, yojson]
end)
type block_and_signatures = {
  signatures : Signatures.t;
  block : Block.t option;
  hash : BLAKE2B.t;
}
type t = {
  self_key : Wallet.t;
  available : block_and_signatures Hash_map.t;
  available_by_previous : block_and_signatures Hash_map.t;
  signed : block_and_signatures Hash_map.t;
  signed_by_previous : (Block.t * Signatures.t) Hash_map.t;
}
let update_block_and_signatures block_and_signatures t =
  let add_to_map __x = Hash_map.add __x block_and_signatures in
  let hash = block_and_signatures.hash in
  let is_signed = Signatures.is_signed block_and_signatures.signatures in
  {
    self_key = t.self_key;
    available = add_to_map hash t.available;
    available_by_previous =
      (match block_and_signatures.block with
      | Some block -> add_to_map block.previous_hash t.available_by_previous
      | None -> t.available_by_previous);
    signed =
      (match is_signed with
      | true -> add_to_map hash t.signed
      | false -> t.signed);
    signed_by_previous =
      (match (is_signed, block_and_signatures.block) with
      | true, Some block ->
        Hash_map.add block.previous_hash
          (block, block_and_signatures.signatures)
          t.signed_by_previous
      | _ -> t.signed_by_previous);
  }
let find_block_and_signature_or_return_empty ~hash t =
  match Hash_map.find_opt hash t.available with
  | Some block_and_signatures -> block_and_signatures
  | None ->
    let signatures = Signatures.make ~self_key:t.self_key in
    { signatures; block = None; hash }
let is_signed block_and_signatures =
  Signatures.is_signed block_and_signatures.signatures
let rec set_signed block_and_signatures t =
  let signatures = Signatures.set_signed block_and_signatures.signatures in
  let block_and_signatures = { block_and_signatures with signatures } in
  let t = update_block_and_signatures block_and_signatures t in
  ensure_previous_is_signed block_and_signatures t

and ensure_previous_is_signed block_and_signatures t =
  match block_and_signatures.block with
  | Some block ->
    let previous_block_and_signatures =
      find_block_and_signature_or_return_empty ~hash:block.previous_hash t in
    if is_signed previous_block_and_signatures then
      t
    else
      set_signed previous_block_and_signatures t
  | None -> t
let append_block block t =
  let block_and_signatures =
    find_block_and_signature_or_return_empty ~hash:block.Block.hash t in
  let block_and_signatures = { block_and_signatures with block = Some block } in
  let block_and_signatures =
    {
      block_and_signatures with
      signatures =
        (if block.hash = Block.genesis.hash then
           Signatures.set_signed block_and_signatures.signatures
        else
          block_and_signatures.signatures);
    } in
  let t = update_block_and_signatures block_and_signatures t in
  if is_signed block_and_signatures then
    ensure_previous_is_signed block_and_signatures t
  else
    t
let append_signature ~signatures_required ~hash signature t =
  let block_and_signatures = find_block_and_signature_or_return_empty ~hash t in
  let block_and_signatures =
    {
      block_and_signatures with
      signatures =
        Signatures.add ~signatures_required signature
          block_and_signatures.signatures;
    } in
  let t = update_block_and_signatures block_and_signatures t in
  if is_signed block_and_signatures then
    ensure_previous_is_signed block_and_signatures t
  else
    t
let is_signed ~hash t = Hash_map.mem hash t.signed
let find_block ~hash t =
  let%some { block; _ } = Hash_map.find_opt hash t.available in
  block
let find_signatures ~hash t =
  let%some { signatures; _ } = Hash_map.find_opt hash t.available in
  Some signatures
let find_next_block_to_apply ~hash t =
  let%some block, _ = Hash_map.find_opt hash t.signed_by_previous in
  Some block
let rec find_all_signed_blocks_above blocks (block, signatures) t =
  match Hash_map.find_opt block.Block.hash t.signed_by_previous with
  | Some (another_block, signatures) ->
    find_all_signed_blocks_above (block :: blocks) (another_block, signatures) t
  | None -> (blocks, (block, signatures))
let find_all_signed_blocks_above = find_all_signed_blocks_above []
let make ~self_key =
  let empty =
    {
      self_key;
      available = Hash_map.empty;
      available_by_previous = Hash_map.empty;
      signed = Hash_map.empty;
      signed_by_previous = Hash_map.empty;
    } in
  append_block Block.genesis empty
