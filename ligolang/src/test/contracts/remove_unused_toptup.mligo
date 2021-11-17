let (x, y) = (1n, 2n)

let main (_, s : unit * int) : operation list * int =
  (([] : operation list), s + x + y)
