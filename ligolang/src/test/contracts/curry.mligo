let conv_test (j : int) (k : int) = j + k
let main (i : int) : int = conv_test i 10
let partial (a : int) (b : int) : int = a + b
let mk_partial (j : int) : int -> int = partial j
let partial_apply (i : int) : int = mk_partial 10 i
