let main (_, _ : unit * unit) : operation list * unit =
  ([] : operation list), ()

let test =
  let (taddr, _, _) = Test.originate main () 0tez in
  let contr = Test.to_contract taddr in
  let addr = Tezos.address contr in
  let () = Test.log addr in
  let () = Test.set_source addr in
  let (_, _, _) = Test.originate main () 0tez in
  ()
