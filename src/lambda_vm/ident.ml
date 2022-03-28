(* TODO: should I care about overflowing this? *)
type t = int [@@deriving yojson]

let compare = Int.compare
let equal = Int.equal
let initial = 0
let next t = t + 1

let to_string = string_of_int

let pp fmt t = Format.pp_print_string fmt (to_string t)
