type storage = int
type parameter = nat

let main (action, store : parameter * storage) : storage =
  store + 1