open Deku_crypto

type block_pool
type t = block_pool

val empty : block_pool
val append_block : block:Block.t -> block_pool -> block_pool

val append_vote :
  validator:Key_hash.t -> hash:Block_hash.t -> block_pool -> block_pool

val remove : block:Block.t -> block_pool -> block_pool
val find_block : hash:Block_hash.t -> block_pool -> Block.t option
val find_votes : hash:Block_hash.t -> block_pool -> Key_hash.Set.t
val find_next : hash:Block_hash.t -> block_pool -> Block.Set.t
