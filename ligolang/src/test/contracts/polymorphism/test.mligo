let id (type a) (x : a) : a = x

let main (m, n : int * int) : operation list * int =
  ([] : operation list), (id n) + (id m)

let test =
  let (taddr, _, _) = Test.originate main 0 0tez in
  let () = Test.transfer_to_contract_exn (Test.to_contract taddr) 42 0tez in
  assert (Test.get_storage taddr = 42)
