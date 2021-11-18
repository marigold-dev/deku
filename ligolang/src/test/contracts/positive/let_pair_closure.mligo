let main (p, _ : (nat * string) * (unit -> unit)) : operation list * (unit -> unit) =
  let (x, y) = p in
  let f = fun (_ : unit) -> failwith (x + x + String.length y) in
  (([] : operation list), f)
