let f : int = fun (x, y : int*int) -> x + y
let g (x, y : int * int) : int = f (x, y)
