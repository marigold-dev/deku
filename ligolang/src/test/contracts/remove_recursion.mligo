let rec f (n : int) : int =
  let rec f (n : int) : int = if (n = 0) then 1 else f (n - 1) in
  f 4

let rec g (f : int -> int) : int -> int =
  g (let rec h (n : int) : int = if (n = 0) then 1 else h (n - 1) in h)
