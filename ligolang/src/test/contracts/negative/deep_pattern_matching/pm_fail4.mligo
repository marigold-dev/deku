type myt = Nil | Cons of (int * int)

let t = fun (x: myt * myt) ->
  match x with
  | Nil , ys  -> 1
  | xs  , Nil -> 2