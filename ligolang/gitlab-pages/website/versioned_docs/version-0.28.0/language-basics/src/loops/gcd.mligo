let iter (x,y : nat * nat) : bool * (nat * nat) =
  if y = 0n then stop (x,y) else continue (y, x mod y)

let gcd (x,y : nat * nat) : nat =
  let x,y = if x < y then y,x else x,y in
  let x,y = Loop.fold_while iter (x,y)
  in x
