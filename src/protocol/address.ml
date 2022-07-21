open Deku_crypto
open Key_hash

type address = Key_hash.t
and t = address [@@deriving eq, ord, yojson]

let of_key_hash key_hash = key_hash
let to_key_hash address = address
let of_b58 = of_b58
let to_b58 = to_b58

module Map = Map.Make (struct
  type t = address

  let compare = compare
end)
