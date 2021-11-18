type myt = Nil | Cons of (int * int)

let t = fun (x: myt * ( int * int * int)) ->
  match x with
  | xs , (a,b,c) -> 1
  | xs , (c,b,a) -> 2