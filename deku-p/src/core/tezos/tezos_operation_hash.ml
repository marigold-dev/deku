open Deku_repr
open Deku_crypto
open BLAKE2b

type tezos_operation_hash = BLAKE2b.t [@@deriving eq, ord]
type t = BLAKE2b.t [@@deriving eq, ord]

include With_b58_and_encoding (struct
  let name = "Operation_hash"
  let prefix = Prefix.operation_hash
end)

module Set = Set
module Map = Map
