type foo = (int, int) big_map

let set_ (n, m: int * foo) : foo = Big_map.update 23 (Some n) m
let add (n, m : int * foo) : foo = Big_map.add 23 n m

let rm (m : foo) : foo = Big_map.remove 42 m
let gf (m : foo) : int = Big_map.find 23 m

let get (m : foo): int option = Big_map.find_opt 42 m

let empty_map : foo = Big_map.empty

let map1 : foo = Big_map.literal [(23,0); (42,0)]
let map1 : foo = Big_map.literal [(23,0); (42,0)]

let mutimaps (m : foo) (n : foo) : foo =
  let bar : foo = Big_map.update 42 (Some 0) m
  in Big_map.update 42 (get bar) n
