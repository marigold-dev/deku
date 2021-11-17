type myt is Nil | Cons of (int * int)

function t (const x: myt * ( int * int * int)) is
  case x of
  | (xs , (a,b,c)) -> 1
  | (xs , (c,b,a)) -> 2
  end