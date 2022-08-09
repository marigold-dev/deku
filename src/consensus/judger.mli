open Deku_crypto

val is_valid : state:State.t -> Block.t -> bool
val is_signable : current:Timestamp.t -> state:State.t -> Block.t -> bool
val is_accepted : state:State.t -> block_pool:Block_pool.t -> Block.t -> bool
val is_producer : current:Timestamp.t -> state:State.t -> Key_hash.t -> bool
