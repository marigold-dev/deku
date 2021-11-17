type toto = { a : int ; b : string }
type tata = { a : int ; }

let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)

let main (p:int) (storage : int) =
  (([] : operation list) , p + foo.a)
