open Deku_concepts
open Deku_crypto

type block_pool = private
  | Pool of {
      by_hash :
        (Block.t option
        * (* FIXME: I think this can just be a set and it will be simpler *)
        Verified_signature.t Key_hash.Map.t)
        Block_hash.Map.t;
      by_previous : Block.Set.t Block_hash.Map.t;
    }

type t = block_pool [@@deriving yojson]

val empty : block_pool
val append_block : block:Block.t -> block_pool -> block_pool

val append_vote :
  vote:Verified_signature.t -> hash:Block_hash.t -> block_pool -> block_pool

val remove : block:Block.t -> block_pool -> block_pool
val find_block : hash:Block_hash.t -> block_pool -> Block.t option

val find_votes :
  hash:Block_hash.t -> block_pool -> Verified_signature.t Key_hash.Map.t

val find_next : hash:Block_hash.t -> block_pool -> Block.Set.t
