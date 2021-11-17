let main (p : key_hash) =
  let c : unit contract = Tezos.implicit_account p
  in Tezos.address c
