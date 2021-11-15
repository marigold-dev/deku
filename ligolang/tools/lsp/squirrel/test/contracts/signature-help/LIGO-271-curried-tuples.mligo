let f (a, b : int * nat) (c : unit) ((d : nat), (e : int)) : int = a

let g : int =
  let t = (1, 2n) in
  f t () (3n, 4)

let h : int =
  let t = (3n, 4) in
  f (1, 2n) () t
