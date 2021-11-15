let k1 (type a b) (w : a) (_x : b) : a = w
let k2 (type c d) (y : c) (_z : d) : c = y
let f (b : bool) (str : string) =
  let k (type e f) : e -> f -> e = if b then (k1 : e -> f -> e) else (k2 : e -> f -> e) in
  k str (40 + 2)

let v = f true "hello"
