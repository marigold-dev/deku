let x (type a) (x : a) : a = x

let app (f : int -> int) (g : int -> int) (x : int) : int = f (g x)

let foo = app (x : int -> int) (fun (x : int) -> x) 1
