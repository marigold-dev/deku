
let c () : operation * address =
  Tezos.create_contract (fun ((_, _) : unit * unit) -> ([] : operation list), ()) (None : key_hash option) 0tez ()

let foo =
  let (_a, _b) : operation * address = c () in
  ()

let c : unit -> int * string * nat * int * string * nat * int * string * nat * int * string =
  fun () -> (1, "1", 1n, 2, "2", 2n, 3, "3", 3n, 4, "4")

let foo =
  let (_i1, _s1, _n1, _i2, _s2, _n2, _i3, _s3, _n3, _i4, _s4) :
   int * string * nat * int * string * nat * int * string * nat * int * string = c () in
  ()
