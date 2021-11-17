let rec main (p, s : unit * unit) : operation list * unit =
  main (p, s)

let test =
  let (taddr, _, _) = Test.originate main () 0tez in
  let contr = Test.to_contract taddr in
  ()
