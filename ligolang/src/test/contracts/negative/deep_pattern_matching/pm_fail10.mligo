type myd = One of int | Two

let t = fun (x : myd) ->
  match x with
  | One 1 -> 2
  | Two -> 1
