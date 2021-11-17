type storage = address
type return = operation list * storage
type parameter = One of address | Two

let main (_, _ : parameter * storage) : operation list * storage =
  let c : parameter contract = Tezos.self "%default" in
  ( ([] : operation list), Tezos.address c )
