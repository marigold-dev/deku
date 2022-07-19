open Deku_stdlib
open N

type level = N.t
type t = level

let show l = show l
let pp fmt l = pp fmt l
let equal a b = equal a b
let compare a b = compare a b
let zero = zero
let next x = x + one
let of_n n = n
let to_n n = n
let ( < ) a b = a < b
