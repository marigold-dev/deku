type toto = (int * string)

let foo : (int * string * bool) = ((1, "foo") : toto)

let main (p:int) (storage : int) =
  (([] : operation list) , p + foo.0)
