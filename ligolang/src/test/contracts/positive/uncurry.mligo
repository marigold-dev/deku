(* only 2 args uncurried *)
let add3 (x : int) (y : int) (z : int) =
  x + y + z

(* all 4 args uncurried *)
let add4 (x : int) (y : int) (z : int) (w : int) =
  x + y + z + w

(* 2 args uncurried *)
let weird2 (_x : int) (f : int -> int) : int -> int = f

(* not uncurried because applied to 3 args *)
let weird2b (_x : int) (f : int -> int) : int -> int = f

(* not uncurried because applied to different numbers of args *)
let diff (x : int) (y : int) (z : int) =
  x + y + z

(* thwarting inlining optimisation below, we should add [@inline never] *)
let main (p, s : int * int) : operation list * int =
  let f1 = add3 p p in
  let s = f1 s + f1 s in
  let f2 = add3 p p in
  let s = f2 s + f2 s in
  let s = add4 p p p s + add4 p p p s in
  let g1 = weird2 p (add3 p p) in
  let s = g1 s + g1 s in
  let g2 = weird2 p (add3 p p) in
  let s = g2 s + g2 s in
  let h1 (s : int) = weird2b p (add3 p p) s in
  let s = h1 s + h1 s + s in
  let h2 (s : int) = weird2b p (add3 p p) s in
  let s = h2 s + h2 s + s in
  let k1 = diff p p in
  let s = k1 s + k1 s in
  let k2 (s : int) = diff p p s in
  let s = k2 s + k2 s in
  (([] : operation list), s)
