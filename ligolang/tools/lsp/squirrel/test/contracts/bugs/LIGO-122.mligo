let main (a, store : int * int) : operation list * int =
    let foo (b : int) : int = b * b in
    ([] : operation list), foo a