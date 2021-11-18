function local_type (var u : unit) : int is block {
	type toto is int;
	var titi : toto := 1;
	titi := titi + 2
} with titi
