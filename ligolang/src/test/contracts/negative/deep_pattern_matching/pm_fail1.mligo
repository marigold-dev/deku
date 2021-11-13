type myt = Nil | Cons of (int * int)
type myr = { a : int ; b : nat ; c : string }

let t = fun (x: myt * myt) ->
  match x with
  | Nil , {a = a ; b = b ; c = c} -> 1
  | xs  , Nil -> 2
  | Cons (a,b) , Cons (c,d) -> a + b + c + d