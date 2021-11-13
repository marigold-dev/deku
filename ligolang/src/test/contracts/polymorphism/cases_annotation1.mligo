let k1 (type a b) (w : a) (_x : b) : a = w
let k2 (type c d) (y : c) (_z : d) : c = y
let f (b : bool) (str : string) =
  let k (type e f) (x : e) (y : f) : e = if b then k1 x y else k2 x y in
  k str (40 + 2)

let v = f true "hello"
