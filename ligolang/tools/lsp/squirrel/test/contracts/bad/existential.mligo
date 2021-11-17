let a : 'a = 2
let b : _ ->'b = fun _ -> 2
let c : 'a -> 'a = fun x -> 2
let d : 'a -> 'b = fun x -> x
let e =
	let a : 'a = 2 in
	let b : 'a = "a" in
	a + b