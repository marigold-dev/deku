open Deku_stdlib

(* FIXME: this isn't yet used. We have to fix this. *)

type nonce
type t = nonce [@@deriving eq, ord, yojson]

(* repr *)
val of_n : N.t -> nonce
val to_n : nonce -> N.t
