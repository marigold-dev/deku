#import "type_A.mligo" "A"

let main ((p,s): (A.titi * A.toto)) =
	let s = s + 1 in
	let p = p ^ "titi" in
	([] : operation list), s
