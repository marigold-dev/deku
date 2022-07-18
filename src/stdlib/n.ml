open Z

type nat = Z.t
type t = nat

let show n = Format.asprintf "%a" Z.pp_print n
let pp fmt n = Z.pp_print fmt n
let equal a b = equal a b
let compare a b = compare a b
let zero = zero
let one = one
let check n = if n < zero then None else Some n
let of_z x = check x
let to_z x = x
let ( + ) a b = a + b
let ( - ) a b = check (a - b)
let ( < ) a b = a < b
