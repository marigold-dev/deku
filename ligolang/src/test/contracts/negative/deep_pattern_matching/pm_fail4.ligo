type myt is Nil | Cons of (int * int)

function t (const x: myt * myt) is
  case x of
  | (Nil , ys)  -> 1
  | (xs  , Nil) -> 2
  end