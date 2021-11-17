let main2 (p : key_hash) (s : unit) =
  let c : unit contract = Tezos.implicit_account p
  in ([] : operation list), unit

let main (p,s : key_hash * unit) = main2 p s
