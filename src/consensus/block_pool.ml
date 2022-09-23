open Deku_concepts
open Deku_crypto
open Block

(* TODO: not accepted blocks and signatures are never removed *)
type block_pool =
  | Pool of {
      by_hash :
        (Block.t option * Verified_signature.t Key_hash.Map.t) Block_hash.Map.t;
      by_previous : Block.Set.t Block_hash.Map.t;
    }

type t = block_pool

(* helpers *)
let find_by_hash hash map =
  match Block_hash.Map.find_opt hash map with
  | Some (block, votes) -> (block, votes)
  | None -> (None, Key_hash.Map.empty)

let find_by_previous hash map =
  match Block_hash.Map.find_opt hash map with
  | Some blocks -> blocks
  | None -> Block.Set.empty

let empty =
  Pool { by_hash = Block_hash.Map.empty; by_previous = Block_hash.Map.empty }

let append_block ~block pool =
  let (Pool { by_hash; by_previous }) = pool in
  let (Block { hash; previous; _ }) = block in
  match find_by_hash hash by_hash with
  | Some _block, _votes -> pool
  | None, votes ->
      let by_hash = Block_hash.Map.add hash (Some block, votes) by_hash in
      let by_previous =
        let blocks = find_by_previous previous by_previous in
        let blocks = Block.Set.add block blocks in
        Block_hash.Map.add previous blocks by_previous
      in
      Pool { by_hash; by_previous }

let append_vote ~vote ~hash pool =
  let (Pool { by_hash; by_previous }) = pool in
  let block, votes = find_by_hash hash by_hash in
  (* FIXME: do we konw for sure this is actually an approved validator? *)
  let validator = Verified_signature.key_hash vote in
  let votes = Key_hash.Map.add validator vote votes in
  let by_hash = Block_hash.Map.add hash (block, votes) by_hash in
  Pool { by_hash; by_previous }

let remove ~block pool =
  let (Pool { by_hash; by_previous }) = pool in
  let (Block { hash; previous; _ }) = block in
  let by_hash = Block_hash.Map.remove hash by_hash in
  let by_previous = Block_hash.Map.remove previous by_previous in
  Pool { by_hash; by_previous }

let find_block ~hash pool =
  let (Pool { by_hash; by_previous = _ }) = pool in
  let block, _votes = find_by_hash hash by_hash in
  block

let find_votes ~hash pool =
  let (Pool { by_hash; by_previous = _ }) = pool in
  let _block, votes = find_by_hash hash by_hash in
  votes

let find_next ~hash pool =
  let (Pool { by_hash = _; by_previous }) = pool in
  find_by_previous hash by_previous

(* yojson *)
(* TODO: this is not safe, because we don't reload the signatures from
   validators*)
let t_of_yojson json =
  let list =
    [%of_yojson:
      (Block_hash.t * Block.t option * Verified_signature.t list) list] json
  in
  List.fold_left
    (fun pool (hash, block, votes) ->
      let hash, pool =
        match block with
        | Some block ->
            let (Block { hash; _ }) = block in
            let pool = append_block ~block pool in
            (hash, pool)
        | None -> (hash, pool)
      in
      List.fold_left (fun pool vote -> append_vote ~vote ~hash pool) pool votes)
    empty list

let yojson_of_t pool =
  let (Pool { by_hash; by_previous = _ }) = pool in
  let list =
    Block_hash.Map.fold
      (fun hash (block, votes) list ->
        let votes = Key_hash.Map.bindings votes |> List.map snd in
        (hash, block, votes) :: list)
      by_hash []
  in
  [%yojson_of: (Block_hash.t * Block.t option * Verified_signature.t list) list]
    list
(*
   let find_signatures hash map =
     match Block_hash.Map.find_opt hash map with
     | Some (signatures, _timestamp) -> signatures
     | None -> Key_hash.Map.empty

   let empty =
     Pool { by_hash = Block_hash.Map.empty; by_previous = Block_hash.Map.empty }

   let append_block ~block pool =
     let (Pool { by_hash; by_previous }) = pool in
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
       let key_hash = Verified_signature.key_hash signature in
       let signatures = Key_hash.Map.add key_hash signature signatures in
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
         Pool { by_hash; by_previous }

   let append_vote ~validator ~hash pool =
     let (Pool { by_hash; by_previous }) = pool in
     let block, votes = find_by_hash hash by_hash in
     let votes = Key_hash.Set.add validator votes in
     let by_hash = Block_hash.Map.add hash (block, votes) by_hash in
     Pool { by_hash; by_previous }

   let remove ~block pool =
     let (Pool { by_hash; by_previous }) = pool in
     let (Block { hash; previous; _ }) = block in
     let by_hash = Block_hash.Map.remove hash by_hash in
     let by_previous = Block_hash.Map.remove previous by_previous in
     Pool { by_hash; by_previous }

   let find_block ~hash pool =
     let (Pool { by_hash; by_previous = _ }) = pool in
     let block, _votes = find_by_hash hash by_hash in
     block

   let find_votes ~hash pool =
     let (Pool { by_hash; by_previous = _ }) = pool in
     let _block, votes = find_by_hash hash by_hash in
     votes

   let find_next ~hash pool =
     let (Pool { by_hash = _; by_previous }) = pool in
     find_by_previous hash by_previous

   (* yojson *)
   (* TODO: this is not safe, because we don't reload the signatures from
      validators*)
   let t_of_yojson json =
     let list =
       [%of_yojson: (Block_hash.t * Block.t option * Key_hash.t list) list] json
     in
     List.fold_left
       (fun pool (hash, block, votes) ->
         let hash, pool =
           match block with
           | Some block ->
               let (Block { hash; _ }) = block in
               let pool = append_block ~block pool in
               (hash, pool)
           | None -> (hash, pool)
         in
         List.fold_left
           (fun pool validator -> append_vote ~validator ~hash pool)
           pool votes)
       empty list

   let yojson_of_t pool =
     let (Pool { by_hash; by_previous = _ }) = pool in
     let list =
       Block_hash.Map.fold
         (fun hash (block, votes) list ->
           let votes = Key_hash.Set.elements votes in
           (hash, block, votes) :: list)
         by_hash []
     in
     [%yojson_of: (Block_hash.t * Block.t option * Key_hash.t list) list] list *)
