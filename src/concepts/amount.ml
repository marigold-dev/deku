open Deku_stdlib
open N

type amount = N.t
and t = amount [@@deriving show, eq, ord, yojson]

let zero = zero
let one = one
let of_n x = x
let to_n x = x
let ( + ) a b = a + b
let ( - ) a b = a - b
let of_int x = N.of_z (Z.of_int x)
