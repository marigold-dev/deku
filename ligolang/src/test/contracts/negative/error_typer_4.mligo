type toto = { a : int ; b : string ; c : bool }
type tata = { a : int ; d : string ; c : bool }

let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)

let main (p:int) (storage : int) =
  (([] : operation list) , p + foo.a)
