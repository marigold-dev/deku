type return = operation list * string

let a : int = 2

let main (action, store : string * string) : return =
  let toto : operation * address = Tezos.create_contract
    (fun (p, s : nat * int) -> (([] : operation list), a)) 
    (None: key_hash option) 
    300tz 
    1 
  in
  ([toto.0], store)