type storage = unit

let main (_, _ : unit * storage) : operation list * storage =
  (failwith "This contract always fails" : operation list * storage)
