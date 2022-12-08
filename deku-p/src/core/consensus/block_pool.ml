open Deku_concepts
open Block

(* TODO: clean block pool *)
(* TODO: not accepted blocks and signatures are never removed *)
type block_pool =
  | Pool of
      (Block.t option * Verified_signature.Set.t) Block_hash.Map.t Level.Map.t

type t = block_pool

(* helpers *)
let find_by_hash hash map =
  match Block_hash.Map.find_opt hash map with
  | Some (block, votes) -> (block, votes)
  | None -> (None, Verified_signature.Set.empty)

let find_by_level level map =
  match Level.Map.find_opt level map with
  | Some blocks -> blocks
  | None -> Block_hash.Map.empty

let empty = Pool Level.Map.empty

let append_block ~block pool =
  let (Block { hash; level; _ }) = block in
  let (Pool by_level) = pool in
  let by_hash = find_by_level level by_level in
  match find_by_hash hash by_hash with
  | Some _block, _votes -> pool
  | None, votes ->
      let by_hash = Block_hash.Map.add hash (Some block, votes) by_hash in
      let by_level = Level.Map.add level by_hash by_level in
      Pool by_level

let append_vote ~level ~vote pool =
  let (Pool by_level) = pool in
  let hash = Verified_signature.signed_hash vote in
  let hash = Block_hash.of_blake2b hash in
  let by_hash = find_by_level level by_level in
  let block, votes = find_by_hash hash by_hash in
  let votes = Verified_signature.Set.add vote votes in
  let by_hash = Block_hash.Map.add hash (block, votes) by_hash in
  let by_level = Level.Map.add level by_hash by_level in
  Pool by_level

let find_block ~level ~hash pool =
  let (Pool by_level) = pool in
  let by_hash = find_by_level level by_level in
  let block, _votes = find_by_hash hash by_hash in
  block

let find_votes ~level ~hash pool =
  let (Pool by_level) = pool in
  let by_hash = find_by_level level by_level in
  let _block, votes = find_by_hash hash by_hash in
  votes

let find_level ~level pool =
  let (Pool by_level) = pool in
  let by_hash = find_by_level level by_level in
  Block_hash.Map.fold
    (fun _hash (block, _votes) blocks ->
      match block with Some block -> block :: blocks | None -> blocks)
    by_hash []

(* TODO: dedup*)
let rec drop ~level ~until by_level =
  match Level.(until > level) with
  | true ->
      let by_level = Level.Map.remove level by_level in
      let level = Level.next level in
      drop ~level ~until by_level
  | false -> Level.Map.remove until by_level

let drop ~until by_level =
  match Level.Map.min_binding_opt by_level with
  | Some (level, _by_hash) -> drop ~level ~until by_level
  | None -> by_level

let close_level ~until pool =
  let (Pool by_level) = pool in
  let by_level = drop ~until by_level in
  Pool by_level

(* TODO: not safe, doesn't check if signatures match validator *)
type repr = {
  blocks : Block.t list;
  votes : (Level.t * Verified_signature.t) list;
}

let of_repr { blocks; votes } =
  let pool =
    List.fold_left (fun pool block -> append_block ~block pool) empty blocks
  in
  List.fold_left
    (fun pool (level, vote) -> append_vote ~level ~vote pool)
    pool votes

let to_repr (Pool by_level) =
  let blocks =
    Level.Map.fold
      (fun _level by_hash blocks ->
        Block_hash.Map.fold
          (fun _hash (block, _votes) blocks ->
            match block with Some block -> block :: blocks | None -> blocks)
          by_hash blocks)
      by_level []
  in
  let votes =
    Level.Map.fold
      (fun level by_hash list ->
        Block_hash.Map.fold
          (fun _hash (_block, votes) list ->
            Verified_signature.Set.fold
              (fun vote list -> (level, vote) :: list)
              votes list)
          by_hash list)
      by_level []
  in
  { blocks; votes }

let repr_encoding =
  let open Data_encoding in
  conv
    (fun { blocks; votes } -> (blocks, votes))
    (fun (blocks, votes) -> { blocks; votes })
    (tup2 (list Block.encoding)
       (list (tup2 Level.encoding Verified_signature.encoding)))

let encoding =
  let open Data_encoding in
  conv to_repr of_repr repr_encoding
