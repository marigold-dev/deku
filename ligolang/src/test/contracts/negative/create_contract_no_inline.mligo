let foo : int = 42

let dummy_contract (p, s : nat * int) : return =
 (([] : operation list), foo)

type return = operation list * string

let main (action, store : int * int) : return =
  let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
  let toto : operation list = [ op ] in
  (toto, foo)
