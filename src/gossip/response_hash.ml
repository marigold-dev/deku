open Deku_repr
open Deku_crypto
open BLAKE2b

type response_hash = BLAKE2b.t
and t = response_hash [@@deriving eq, ord]

let to_blake2b response_hash = response_hash

include With_b58 (struct
  let prefix = Prefix.deku_response_hash
end)

include With_yojson_of_b58 (struct
  type t = response_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let hash = hash

module Set = Set
module Map = Map

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
