type myt = Nil | Cons of (int * int)

let t = fun (x: myt * myt) ->
  match x with
  | Nil , (a,b,c) -> 1
  | xs  , Nil -> 2
  | Cons (a,b) , Cons (c,d) -> a + b + c + d