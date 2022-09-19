open Deku_repr
open Deku_crypto
open BLAKE2b

type operation_hash = BLAKE2b.t
and t = operation_hash [@@deriving eq, ord]

let to_blake2b operation_hash = operation_hash
let of_blake2b operation_hash = operation_hash

include With_b58 (struct
  let prefix = Prefix.deku_operation_hash
end)

include With_yojson_of_b58 (struct
  type t = operation_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let hash = hash

module Map = Map
