type toto = int option

let foo : string list = Some (42 + 127)

let main (p:int) (storage : int) =
  (([] : operation list) , p)
