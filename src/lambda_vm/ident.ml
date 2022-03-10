(* TODO: should I care about overflowing this? *)
type t = int [@@deriving eq, show]

let compare = Int.compare
let initial = 0
let next t = t + 1
