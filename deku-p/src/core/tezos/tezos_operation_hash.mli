open Deku_stdlib

type tezos_operation_hash [@@deriving eq, ord, yojson]
type t = tezos_operation_hash [@@deriving eq, ord, yojson]

val of_b58 : string -> tezos_operation_hash option
val to_b58 : tezos_operation_hash -> string
val encoding : tezos_operation_hash Data_encoding.t

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
