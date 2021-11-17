function gcd (var x : nat; var y : nat) : nat is block {
  if x < y then
    block {
      const z : nat = x;
      x := y; y := z
    }
  else skip;
  var r : nat := 0n;
  while y =/= 0n block {
    r := x mod y;
    x := y;
    y := r
  }
} with x
