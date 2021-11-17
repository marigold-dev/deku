let local_type = (_ : unit) : int => {
	type toto = int;
	let titi : toto = 1;
	let titi = titi + 2;
	titi
};
