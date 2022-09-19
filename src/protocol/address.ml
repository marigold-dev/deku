open Deku_crypto
open Key_hash

type address = Key_hash.t
and t = address [@@deriving eq, ord, yojson]

let of_key_hash key_hash = key_hash
let to_key_hash address = address
let of_b58 = of_b58
let to_b58 = to_b58

module Map = Stdlib.Map.Make (struct
  type t = address

  let compare = compare
end)

let cmdliner_converter =
  let of_string s =
    match of_b58 s with
    | Some s -> `Ok s
    | None ->
        `Error (Format.sprintf "Could not parse '%s' as a Tezos address" s)
  in
  let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
  (of_string, to_string)
