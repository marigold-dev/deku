type myd = One of int | Two

let t = fun (x : myd) ->
  match x with
  | One a -> 2
  | Two -> a
