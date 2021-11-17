let local_type (u : unit) : int =
	type toto = int in
	let titi : toto = 1 in
	let titi = titi + 2 in
	titi
