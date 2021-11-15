
let f (p : unit) : unit = p

let main (p, s : unit * unit) : (operation list) * unit =
  ([] : operation list), f ()
