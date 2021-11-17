let a =
  let f : (int-> int -> int) = fun (i : int) (j : int) -> j + i in
  (let b = 1 in f 1) (let c = 2 in c)