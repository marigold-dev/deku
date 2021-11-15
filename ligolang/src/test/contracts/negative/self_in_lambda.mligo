let foo (u : unit) : address = Tezos.address (Tezos.self "%default" : unit contract)

let main (ps: unit * address): (operation list * address) =
  let dummy = foo () in (* force not to inline foo *)
  ( ([] : operation list) , foo ())
