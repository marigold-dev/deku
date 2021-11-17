let div = (a, b : nat * nat) : option (nat) =>
  if (b == 0n) { None; } else { Some (a/b); }