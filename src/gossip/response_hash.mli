open Deku_stdlib
open Deku_crypto

type response_hash
type t = response_hash [@@deriving eq, ord, yojson]

(* repr *)
val to_blake2b : response_hash -> BLAKE2b.t
val of_b58 : string -> response_hash option
val to_b58 : response_hash -> string

(* operations *)
val hash : string -> response_hash

module Set : Set.S with type elt = response_hash
module Map : Map.S with type key = response_hash
