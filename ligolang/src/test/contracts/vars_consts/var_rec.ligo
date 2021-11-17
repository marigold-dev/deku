recursive function sum (var n : int; const acc: int) : int is
  block {
    var r := 0;
    if n < 1 then
      r := acc
    else
      r := sum(n-1, acc + n)
  } with r
