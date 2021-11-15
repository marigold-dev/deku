type storage = (int,"foo",string,"bar") michelson_or 
type foobar = (int,"baz", int, "fooo" ) michelson_or

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = (M_right ("one") : storage) in
  let bar = (M_right 1 : foobar) in
  (([] : operation list), (foo: storage))
