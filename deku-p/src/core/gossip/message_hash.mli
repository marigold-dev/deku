open Deku_stdlib
open Deku_crypto

type message_hash
type t = message_hash [@@deriving eq, ord, show]

(* repr *)
val to_blake2b : message_hash -> BLAKE2b.t
val of_blake2b : BLAKE2b.t -> message_hash
val of_b58 : string -> message_hash option
val to_b58 : message_hash -> string
val encoding : message_hash Data_encoding.t

(* operations *)
val hash : string -> message_hash

module Set : Set.S with type elt = message_hash
module Map : Map.S with type key = message_hash
