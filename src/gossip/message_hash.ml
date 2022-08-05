open Deku_repr
open Deku_crypto
open BLAKE2b

type message_hash = BLAKE2b.t
and t = message_hash [@@deriving eq, ord]

let to_blake2b message_hash = message_hash

include With_encodings (struct
  let prefix = Prefix.deku_message_hash
end)

let hash = hash

module Set = Set
module Map = Map
