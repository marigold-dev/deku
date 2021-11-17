let main (_, _ : unit * (unit -> address)) : operation list * (unit -> address) =
  (([] : operation list), (fun (_ : unit) -> Tezos.self_address))
