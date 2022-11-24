open Deku_repr
open Deku_crypto
open BLAKE2b

type message_hash = BLAKE2b.t
and t = message_hash [@@deriving eq, ord]

let to_blake2b message_hash = message_hash
let of_blake2b message_hash = message_hash

include With_b58_and_encoding (struct
  let name = "Deku_gossip.Message_hash"
  let prefix = Prefix.deku_message_hash
end)

let hash = hash

module Set = Set
module Map = Map

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
