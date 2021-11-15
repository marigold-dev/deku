let k1 (type a b) (w : a) (_x : b) : a = w
let k2 (type c d) (y : c) (_z : d) : c = y
let f (b : bool) (str : string) =
  let k = if b then k1 else k2 in
  k str (40 + 2)
