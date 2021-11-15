type storage = timestamp
type return = operation list * storage
type parameter = unit

let test_ts = ("2000-01-01t10:10:10Z" : timestamp)

let main (action, store : parameter * storage) : return =
  (([] : operation list), Tezos.now)
