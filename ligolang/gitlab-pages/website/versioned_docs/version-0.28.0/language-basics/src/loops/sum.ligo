function sum (var n : nat) : int is block {
  var acc : int := 0;
  for i := 1 to int (n) block {
    acc := acc + i
  }
} with acc