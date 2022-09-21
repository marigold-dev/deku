open Deku_crypto
open Block

(* TODO: not accepted blocks and signatures are never removed *)

type block_pool =
  | Pool of {
      by_hash : (Block.t option * Key_hash.Set.t) Block_hash.Map.t;
      by_previous : Block.Set.t Block_hash.Map.t;
    }

type t = block_pool

(* helpers *)
let find_by_hash hash map =
  match Block_hash.Map.find_opt hash map with
  | Some (block, votes) -> (block, votes)
  | None -> (None, Key_hash.Set.empty)

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
  [%yojson_of: (Block_hash.t * Block.t option * Key_hash.t list) list] list
