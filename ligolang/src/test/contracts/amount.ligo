function check (const _ : unit) : int is
  block {
    var result : int := 0;
    if amount = 100tez then result := 42 else result := 0
  } with result
