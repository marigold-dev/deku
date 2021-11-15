function increment (var start : nat) : nat is
  block {
   var result : nat := start;
   result := result + 1;
  } with result
