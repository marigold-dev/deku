open Deku_stdlib
open Deku_crypto
open Deku_concepts

type block_hash
type t = block_hash [@@deriving eq, ord, yojson, show]

(* repr *)
(* TODO: this API is not ideal *)
val of_blake2b : BLAKE2b.t -> block_hash
val to_blake2b : block_hash -> BLAKE2b.t
val of_b58 : string -> block_hash option
val to_b58 : block_hash -> string
val encoding : block_hash Data_encoding.t

(* operations *)
val hash :
  block_level:Level.level ->
  block_payload_hash:BLAKE2b.t ->
  state_root_hash:BLAKE2b.t ->
  withdrawal_handles_hash:BLAKE2b.t ->
  t

module Set : Set.S with type elt = block_hash
module Map : Map.S with type key = block_hash
