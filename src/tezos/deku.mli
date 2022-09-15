open Deku_crypto
open Deku_concepts

module Consensus : sig
  val hash_block : block_level:Level.t -> block_content:BLAKE2b.t -> BLAKE2b.t
end
