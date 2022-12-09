open Deku_repr
open Deku_crypto
open BLAKE2b

type operation_hash = BLAKE2b.t
and t = operation_hash [@@deriving eq, ord]

let to_blake2b operation_hash = operation_hash

include With_b58_and_encoding (struct
  let name = "Deku_protocol.Operation_hash"
  let prefix = Prefix.deku_operation_hash
end)

let hash = hash

module Map = Map

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
