(* These are local helpers *)
let append (type a) (xs : a set) (ys : a set) : a set =
  Set.fold (fun ((r, e) : (a set * a)) -> Set.add e r) xs ys
let concat (type b) (xss : (b set) set) : b set =
  Set.fold (fun ((r, l) : (b set * b set)) -> (append l r : b set)) xss (Set.empty : b set)
let map (type a b) (f : a -> b) (xs : a set) : b set =
  Set.fold (fun ((r, e) : (b set * a)) -> Set.add (f e) r) xs (Set.empty : b set)
let concat_map (type a b) (f : a -> b set) (xs : a set) : b set =
  Set.fold (fun ((r, e) : (b set * a)) -> append (f e) r) xs (Set.empty : b set)

type 'a monad = 'a set

let ret (type a) (x : a) : a monad = Set.literal [x]
let bind (type a b) (v : a monad) (f : a -> b monad) : b monad =
  concat_map f v
let mzero (type a) : a monad = Set.empty
let mplus (type a) (xs : a monad) (ys : a monad) : a monad = append xs ys
