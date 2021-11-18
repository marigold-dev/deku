type union_a =
| Add of int
| Remove of int

(* comment this type out to successfully compile the contract *)
type union_b =
| Add of nat
| Remove of nat
| Config of nat

let foo (a : union_a) =
  match a with
  |Add a -> unit
  |Remove b -> unit


let main(p, s : union_a * unit) : (operation list) * unit =
  let ss = foo p in 
  ([]: operation list), ss