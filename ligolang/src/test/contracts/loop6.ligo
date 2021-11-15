// Test while loops in PascaLIGO

function for_sum_step (var n : nat) : int is
  block {
    var acc : int := 0;
    for i := 1 to int (2n*n) step 2
      block {
        acc := acc + i
      }
  } with acc
