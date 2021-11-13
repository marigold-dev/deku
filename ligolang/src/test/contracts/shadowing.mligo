type a = int

type a = A of a | B of a

let main (a : a) =
  let a : a = a in 
  let a = match a with 
    A a -> a
  | B a -> a
  in
  type a = a list in
  let a : a = [A a] in 
  a