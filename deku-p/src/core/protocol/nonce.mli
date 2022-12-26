open Deku_stdlib

type nonce
type t = nonce [@@deriving eq, ord, show]

(* repr *)
val of_n : N.t -> nonce
val to_n : nonce -> N.t
val encoding : nonce Data_encoding.t
