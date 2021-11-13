(* This example demonstrates the compiler's handling of Edo combs *)

type param =
  [@layout:comb]
  { x : int;
    y : int;
    z : int;
    w : int }

let main (p, s : param * int) : operation list * int =
  let { x = x; y = y; z = z; w = w } = p in
  (([] : operation list), x + y + z + w)
