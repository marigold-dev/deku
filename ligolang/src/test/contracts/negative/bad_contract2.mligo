type storage = int
type parameter = nat
type return = string * storage

let main (action, store : parameter * storage) : return =
  ("bad",store + 1)