let id (type a) (x : a) : a = x

let main (_, n : unit * int) : operation list * int =
  ([] : operation list), id n
