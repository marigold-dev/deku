type param =
  | [@annot:foo] A of unit
  | B of unit

let main (_,_ : param * unit) : operation list * unit =
  let c = (Tezos.self("%foo") : unit contract) in
  let op = Tezos.transaction () 0mutez c in
  ([op] : operation list), ()
  
