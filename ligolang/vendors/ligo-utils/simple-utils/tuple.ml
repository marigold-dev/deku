let map_h_2 f g (a , b) = (f a , g b)
let map2 f (a, b) = (f a, f b)
let map3 f (a , b , c) = (f a , f b , f c)
let apply2 f (a, b) = f a b
let list2 (a, b) = [a;b]

module Pair = struct
  let map = map2
  let apply f (a, b) = f a b
end
