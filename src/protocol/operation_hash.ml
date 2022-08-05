open Deku_repr
open Deku_crypto
open BLAKE2b

type operation_hash = BLAKE2b.t
and t = operation_hash [@@deriving eq, ord]

let to_blake2b operation_hash = operation_hash

include With_encodings (struct
  let prefix = Prefix.deku_operation_hash
end)

let hash = hash

module Map = Map
