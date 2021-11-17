let compare cmpa cmpb cmpc (a1,b1,c1) (a2,b2,c2) = match cmpa a1 a2 with 0 -> (match cmpb b1 b2 with 0 -> cmpc c1 c2 | c -> c)| c -> c
let map ~f (a,b,c) = (f a, f b, f c)
let fold ~f ~init (a,b,c) = f (f (f init a) b) c
let fold_map ~f ~init (a,b,c) =
  let init,a = f init a in
  let init,b = f init b in
  let init,c = f init c in
  init,(a,b,c)
