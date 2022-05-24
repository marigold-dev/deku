open Crypto
open Protocol

type block_and_signatures = private {
  signatures : Signatures.t;
  block : Block.t option;
  hash : BLAKE2B.t;
}

type t

val make : self_key:Wallet.t -> t

val append_block : Block.t -> t -> t

val append_signature :
  signatures_required:int -> hash:BLAKE2B.t -> Signature.t -> t -> t

val is_signed : hash:BLAKE2B.t -> t -> bool

val find_block : hash:BLAKE2B.t -> t -> Block.t option

val find_signatures : hash:BLAKE2B.t -> t -> Signatures.t option

val find_next_block_to_apply : hash:BLAKE2B.t -> t -> Block.t option

val find_all_signed_blocks_above :
  Block.t * Signatures.t -> t -> Block.t list * (Block.t * Signatures.t)
