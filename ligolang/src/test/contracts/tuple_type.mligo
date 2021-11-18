let g (b : int) = b + 3

let f (_ : int * int) : int -> int = g

let a (b : int * int -> int -> int) : int = (b (5,3)) 5

let test1 (_: int) = a f

let n (a, b : int * int) : int = a + b

let o (p : int * int -> int) : int = p (3, 9)

let test2 (_ignore : int) = o (n)
