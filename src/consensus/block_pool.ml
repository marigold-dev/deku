open Deku_concepts
open Deku_crypto
open Block

(* TODO: old signatures and blocks are never removed *)
type block_pool = {
  block_by_hash : Block.t Block_hash.Map.t;
  blocks_by_previous : Block.Set.t Block_hash.Map.t;
  signatures_by_hash : Verified_signature.t Key_hash.Map.t Block_hash.Map.t;
}

type t = block_pool

(* helpers *)
let find_blocks hash map =
  match Block_hash.Map.find_opt hash map with
  | Some blocks -> blocks
  | None -> Block.Set.empty

let find_signatures hash map =
  match Block_hash.Map.find_opt hash map with
  | Some signatures -> signatures
  | None -> Key_hash.Map.empty

let empty =
  {
    block_by_hash = Block_hash.Map.empty;
    blocks_by_previous = Block_hash.Map.empty;
    signatures_by_hash = Block_hash.Map.empty;
  }

let append_block block pool =
  let { block_by_hash; blocks_by_previous; signatures_by_hash } = pool in
  let (Block { hash; previous; _ }) = block in
  let block_by_hash = Block_hash.Map.add hash block block_by_hash in
  let blocks_by_previous =
    let blocks = find_blocks previous blocks_by_previous in
    let blocks = Block.Set.add block blocks in
    Block_hash.Map.add previous blocks blocks_by_previous
  in
  { block_by_hash; blocks_by_previous; signatures_by_hash }

let append_signature signature pool =
  let { block_by_hash; blocks_by_previous; signatures_by_hash } = pool in
  let signature_hash = Verified_signature.signed_hash signature in
  let signature_hash = Block_hash.of_blake2b signature_hash in
  let signatures_by_hash =
    let signatures = find_signatures signature_hash signatures_by_hash in
    let key_hash = Verified_signature.key_hash signature in
    let signatures = Key_hash.Map.add key_hash signature signatures in
    Block_hash.Map.add signature_hash signatures signatures_by_hash
  in
  { block_by_hash; blocks_by_previous; signatures_by_hash }

let find_block ~block_hash pool =
  let { block_by_hash; blocks_by_previous = _; signatures_by_hash = _ } =
    pool
  in
  Block_hash.Map.find_opt block_hash block_by_hash

let find_next_blocks ~block_previous pool =
  let { block_by_hash = _; blocks_by_previous; signatures_by_hash = _ } =
    pool
  in
  find_blocks block_previous blocks_by_previous

let find_signatures ~block_hash pool =
  let { block_by_hash = _; blocks_by_previous = _; signatures_by_hash } =
    pool
  in
  find_signatures block_hash signatures_by_hash
