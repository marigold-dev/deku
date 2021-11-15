type myt is Nil | Cons of (int * int)

function t (const x: myt * myt) is
  case x of
  | (Nil , (a,b,c)) -> 1
  | (xs  , Nil) -> 2
  | (Cons (a,b) , Cons (c,d)) -> a + b + c + d
  end