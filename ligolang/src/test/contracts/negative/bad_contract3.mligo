type storage = int
type parameter = nat
type return = operation list * string

let main (action, store : parameter * storage) : return =
  (([]: operation list),"bad")