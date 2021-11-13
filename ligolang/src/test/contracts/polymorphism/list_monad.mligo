(* These are local helpers *)
let append (type a) (xs : a list) (ys : a list) : a list =
  List.fold_right (fun ((e, r) : (a * a list)) -> e :: r) xs ys
let concat (type b) (xss : (b list) list) : b list =
  List.fold_right (fun ((l, r) : (b list * b list)) -> (append l r : b list)) xss ([] : b list)

type 'a monad = 'a list

let ret (type a) (x : a) : a monad = [x]
let bind (type a b) (v : a monad) (f : a -> b monad) : b monad =
  concat (List.map f v)
let mzero (type a) : a monad = []
let mplus (type a) (xs : a monad) (ys : a monad) : a monad = append xs ys
