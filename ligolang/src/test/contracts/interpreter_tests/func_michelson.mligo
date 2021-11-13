let michelson_add : int * int -> int =
  [%Michelson ({| { UNPAIR ; ADD } |} : int * int -> int) ]

let main (x, s : int * int) : operation list * int =
  ([] : operation list), michelson_add (x, s)

let test =
  let (taddr, _, _) = Test.originate main 1 0tez in
  let c = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn c 41 0tez in
  Test.log (Test.get_storage taddr)
