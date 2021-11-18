// Test while loops in PascaLIGO

function while_sum (var n : nat) : nat is
  block {
    var i : nat := 0n;
    var r : nat := 0n;
    while i < n block {
      i := i + 1n;
      r := r + i
    };
    var _ := i;
  } with r
