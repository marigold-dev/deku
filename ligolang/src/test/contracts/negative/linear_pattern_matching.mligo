type foo = { a : int ; b : nat ; c : string }

let yy : string = match { a = 1 ; b = 2n ; c = "33" } with
  | { a = a ;  b = b ; c = c } -> a