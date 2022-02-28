(* TODO: should I care about overflowing this? *)
type t = int

let compare = Int.compare
let initial = 0
let next t = t
