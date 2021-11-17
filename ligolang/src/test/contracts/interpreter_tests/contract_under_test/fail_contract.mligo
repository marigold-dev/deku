type storage = unit
type return = operation list * storage
type parameter = unit

let fail_data = "my contract always fail"

let main (action, store : parameter * storage) : return =
  let a = failwith fail_data in
  (([] : operation list), ())
