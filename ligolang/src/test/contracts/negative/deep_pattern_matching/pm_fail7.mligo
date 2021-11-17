type foo = A | B

let t = fun (x: foo) ->
  match x with
  | A -> "hey"
  | B -> 2