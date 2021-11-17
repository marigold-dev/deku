let main (_, _ : unit * unit) : operation list * unit =
  let v = (failwith "foo" : unit) in
  ([] : operation list), ()

let make_call (contr : unit contract) =
  let _ = Test.get_storage_of_address ("tz1fakefakefakefakefakefakefakcphLA5" : address) in
  Test.transfer_to_contract_exn contr () 10tez


let test =
  let (ta, _, _) = Test.originate main () 1tez in
  make_call (Test.to_contract ta)
