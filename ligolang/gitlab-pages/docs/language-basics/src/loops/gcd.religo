let iter = ((x,y) : (nat, nat)) : (bool, (nat, nat)) =>
  if (y == 0n) { stop ((x,y)); } else { continue ((y, x mod y)); };

let gcd = ((x,y) : (nat, nat)) : nat =>
  let (x,y) = if (x < y) { (y,x); } else { (x,y); };
  let (x,y) = Loop.fold_while (iter, (x,y));
  x;
