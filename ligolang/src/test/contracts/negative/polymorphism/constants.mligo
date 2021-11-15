type ('a, 'b) foo = ('a, 'b) map

let merge (type a) (x : (a, a) foo) : bool = false

let m = merge (Map.empty : (int, string) foo)
