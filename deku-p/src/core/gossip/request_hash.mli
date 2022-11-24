open Deku_stdlib
open Deku_crypto

type request_hash
type t = request_hash [@@deriving eq, ord, show]

(* repr *)
val to_blake2b : request_hash -> BLAKE2b.t
val of_b58 : string -> request_hash option
val to_b58 : request_hash -> string

(* operations *)
val hash : string -> request_hash

module Set : Set.S with type elt = request_hash
module Map : Map.S with type key = request_hash
