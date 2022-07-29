open Deku_crypto

type packet_hash
type t = packet_hash [@@deriving eq, ord, yojson]

(* repr *)
val to_blake2b : packet_hash -> BLAKE2b.t
val of_b58 : string -> packet_hash option
val to_b58 : packet_hash -> string

(* operations *)
val hash : string -> packet_hash

module Set : Set.S with type elt = packet_hash
module Map : Map.S with type key = packet_hash
