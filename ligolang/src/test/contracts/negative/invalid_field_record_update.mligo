type abc = {a : int; b : int; c : int}

let main (p:int) (storage : abc) =
  (([] : operation list) , { storage with nofield=2048} )