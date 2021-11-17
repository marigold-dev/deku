type foo = { a :int ; b : nat }

let t1 =
  let { a = a ; f = b }  = { a = 1 ; b = 1n } in
  (a,b)