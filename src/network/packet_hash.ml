open Deku_repr
open Deku_crypto
open BLAKE2b

type packet_hash = BLAKE2b.t
and t = packet_hash [@@deriving eq, ord]

let to_blake2b packet_hash = packet_hash

include With_b58 (struct
  let prefix = Prefix.deku_packet_hash
end)

include With_yojson_of_b58 (struct
  type t = packet_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let hash = hash

module Set = Set
module Map = Map
