open Deku_concepts

type block_pool
type t = block_pool

val empty : block_pool
val append_block : Block.t -> block_pool -> block_pool
val append_signature : Verified_signature.t -> block_pool -> block_pool
val find_block : block_hash:Block_hash.t -> block_pool -> Block.t option
val find_next_blocks : block_previous:Block_hash.t -> block_pool -> Block.Set.t

val find_signatures :
  block_hash:Block_hash.t -> block_pool -> Verified_signature.Set.t
