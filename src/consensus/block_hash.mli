open Deku_crypto
open Deku_concepts

type block_hash
type t = block_hash [@@deriving eq, ord, yojson]

(* repr *)
(* TODO: this API is not ideal *)
val of_blake2b : BLAKE2b.t -> block_hash
val to_blake2b : block_hash -> BLAKE2b.t
val of_b58 : string -> block_hash option
val to_b58 : block_hash -> string

(* operations *)
val hash : block_level:Level.t -> block_content:BLAKE2b.t -> block_hash

(* canonical *)
val genesis : block_hash

module Map : Map.S with type key = block_hash
