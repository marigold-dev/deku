let a = 1

let b =
  let f : (int-> int -> int) = fun (i : int) (j : int) -> j + i in
  f 2 3