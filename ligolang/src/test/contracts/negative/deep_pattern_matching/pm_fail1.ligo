type myt is Nil | Cons of (int * int)
type myr is record [ a : int ; b : nat ; c : string ]

function t (const x: myt * myt) is
  case x of
  | (Nil , record [a ; b ; c ]) -> 1
  | (xs  , Nil) -> 2
  | (Cons (a,b) , Cons (c,d)) -> a + b + c + d
  end