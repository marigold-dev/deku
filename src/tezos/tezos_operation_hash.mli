open Deku_crypto

type tezos_operation_hash = BLAKE2b.t
type t = tezos_operation_hash [@@deriving eq, ord, yojson]

val of_b58 : string -> t option
val to_b58 : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
