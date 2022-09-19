open Deku_concepts
open Block

(* TODO: old signatures and blocks are never removed *)
type block_pool = {
  block_by_hash : (Block.t * Timestamp.t) Block_hash.Map.t;
  blocks_by_previous : (Block.Set.t * Timestamp.t) Block_hash.Map.t;
  signatures_by_hash :
    (Verified_signature.Set.t * Timestamp.t) Block_hash.Map.t;
  last_cleanup : Timestamp.t;
}

type t = block_pool

(* helpers *)
let find_blocks hash map =
  match Block_hash.Map.find_opt hash map with
  | Some (blocks, _timestamp) -> blocks
  | None -> Block.Set.empty

let find_signatures hash map =
  match Block_hash.Map.find_opt hash map with
  | Some (signatures, _timestamp) -> signatures
  | None -> Verified_signature.Set.empty

let empty =
  {
    block_by_hash = Block_hash.Map.empty;
    blocks_by_previous = Block_hash.Map.empty;
    signatures_by_hash = Block_hash.Map.empty;
    last_cleanup = Timestamp.of_float 0.0;
  }

let append_block ~current block pool =
  let { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup } =
    pool
  in
  let (Block { hash; previous; _ }) = block in
  let block_by_hash = Block_hash.Map.add hash (block, current) block_by_hash in
  let blocks_by_previous =
    let blocks = find_blocks previous blocks_by_previous in
    let blocks = Block.Set.add block blocks in
    Block_hash.Map.add previous (blocks, current) blocks_by_previous
  in
  { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup }

let append_signature ~current signature pool =
  let { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup } =
    pool
  in
  let signature_hash = Verified_signature.signed_hash signature in
  let signature_hash = Block_hash.of_blake2b signature_hash in
  let signatures_by_hash =
    let signatures = find_signatures signature_hash signatures_by_hash in
    let signatures = Verified_signature.Set.add signature signatures in
    Block_hash.Map.add signature_hash (signatures, current) signatures_by_hash
  in
  { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup }

let find_block ~block_hash pool =
  let {
    block_by_hash;
    blocks_by_previous = _;
    signatures_by_hash = _;
    last_cleanup = _;
  } =
    pool
  in
  match Block_hash.Map.find_opt block_hash block_by_hash with
  | Some (block, _timestamp) -> Some block
  | None -> None

let find_next_blocks ~block_previous pool =
  let {
    block_by_hash = _;
    blocks_by_previous;
    signatures_by_hash = _;
    last_cleanup = _;
  } =
    pool
  in
  find_blocks block_previous blocks_by_previous

let find_signatures ~block_hash pool =
  let {
    block_by_hash = _;
    blocks_by_previous = _;
    signatures_by_hash;
    last_cleanup = _;
  } =
    pool
  in
  find_signatures block_hash signatures_by_hash

let clean ~current pool =
  let { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup } =
    pool
  in

  (* Format.printf "pool.clean\n%!"; *)
  match
    Timestamp.to_float current -. Timestamp.to_float last_cleanup
    > Deku_constants.clean_block_pool_time
  with
  | true ->
      (* Format.printf "pool.cleaning: %d %d %d\n%!"
         (Block_hash.Map.cardinal block_by_hash)
         (Block_hash.Map.cardinal blocks_by_previous)
         (Block_hash.Map.cardinal signatures_by_hash); *)
      let filter map =
        Block_hash.Map.filter
          (fun _hash (_block, timestamp) ->
            let current = Timestamp.to_float current in
            let timestamp = Timestamp.to_float timestamp in
            let time_since = current -. timestamp in
            time_since < Deku_constants.clean_block_pool_time)
          map
      in
      let block_by_hash = filter block_by_hash in
      let blocks_by_previous = filter blocks_by_previous in
      let signatures_by_hash = filter signatures_by_hash in
      let last_cleanup = current in
      (* Format.printf "pool.cleaned: %d %d %d\n%!"
         (Block_hash.Map.cardinal block_by_hash)
         (Block_hash.Map.cardinal blocks_by_previous)
         (Block_hash.Map.cardinal signatures_by_hash); *)
      { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup }
  | false ->
      { block_by_hash; blocks_by_previous; signatures_by_hash; last_cleanup }
