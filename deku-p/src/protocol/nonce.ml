open Deku_stdlib

(* TODO: should we prefix level in b58? *)
type nonce = N.t
and t = nonce [@@deriving eq, ord, yojson, show]

(* repr *)
let of_n n = n
let to_n nonce = nonce
