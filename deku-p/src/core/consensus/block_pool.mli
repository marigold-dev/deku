open Deku_concepts

type block_pool = private
  | Pool of
      (Block.t option * Verified_signature.Set.t) Block_hash.Map.t Level.Map.t

type t = block_pool

val encoding : block_pool Data_encoding.t
val empty : block_pool
val append_block : block:Block.t -> block_pool -> block_pool

val append_vote :
  level:Level.t -> vote:Verified_signature.t -> block_pool -> block_pool

val find_block :
  level:Level.t -> hash:Block_hash.t -> block_pool -> Block.t option

val find_votes :
  level:Level.t -> hash:Block_hash.t -> block_pool -> Verified_signature.Set.t

val find_level : level:Level.t -> block_pool -> Block.t list
val close_level : until:Level.t -> block_pool -> block_pool
