// Test while loops in PascaLIGO

// recursive to test a bugfix
recursive function counter (var n : nat) : nat is
  block {
    var i : nat := 0n;
    while i < n block {
      i := i + 1n
    }
  } with i
