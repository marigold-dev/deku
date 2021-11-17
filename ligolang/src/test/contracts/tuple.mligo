type abc = int * int * int

let projection_abc (tpl : abc) : int = tpl.1

type foobar = int * int

let fb : foobar = (0,0)

let projection (tpl : foobar) : int = tpl.0 + tpl.1

type big_tuple = int * int * int * int * int

let br : big_tuple = (23, 23, 23, 23, 23)
