open Deku_repr
open Deku_crypto
open BLAKE2b

type tezos_operation_hash = BLAKE2b.t [@@deriving eq, ord, yojson]
type t = BLAKE2b.t [@@deriving eq, ord, yojson]

include With_b58_and_encoding (struct
  let name = "Operation_hash"
  let prefix = Prefix.operation_hash
end)

include With_yojson_of_b58 (struct
  type t = tezos_operation_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

module Set = Set
module Map = Map
