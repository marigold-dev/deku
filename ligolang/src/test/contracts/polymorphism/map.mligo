let map (type a b) (f : a -> b) (x : a list) : b list = List.map f x

let funcs (type a) : (a -> a) list = [(fun (x : a) -> x); (fun (x : a) -> x)]

let uhms (type a b) : (b -> a -> a) list = map (fun (x : a -> a) -> (fun (_ : b) -> x)) (funcs : (a -> a) list)
