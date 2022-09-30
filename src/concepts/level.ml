open Deku_stdlib
open N

(* TODO: should we prefix level in b58? *)
type level = N.t
and t = level [@@deriving show, eq, ord, yojson]

let zero = zero
let next x = x + one
let of_n n = n
let to_n n = n
let ( > ) = ( > )

module Map = Map
