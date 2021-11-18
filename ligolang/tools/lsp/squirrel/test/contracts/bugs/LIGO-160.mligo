type q = { a : int; b : int }
type w = { c : int; d : q }
let foo : q =
  let x : q = { a = 1; b = 2 } in
  let y : w = { c = 5; d = x } in
  { y.d with a = 5 }  // The problem is in "y.d"
