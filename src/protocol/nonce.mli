open Deku_stdlib

type nonce
type t = nonce [@@deriving eq, ord, yojson]

(* repr *)
val of_n : N.t -> nonce
val to_n : nonce -> N.t
