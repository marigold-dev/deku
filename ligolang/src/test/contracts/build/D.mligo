#import "C.mligo" "C"
#import "E.mligo" "E"

let toto = E.toto + C.B.titi

let fb : E.F.foobar = {
	titi = 1;
	toto = toto;
	tata = 2;
	tete = 3;
}

let main ((p,s) : int * int) =
	let s = p + s + toto in
	([]: operation list),s
