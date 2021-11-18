(* these should give a missing type annotation error *)
let a b = b
let a (b,c) = b
let a ((b)) = b
let a = fun b -> b
let a = fun (b,c) -> b
let a = fun ((b)) -> b