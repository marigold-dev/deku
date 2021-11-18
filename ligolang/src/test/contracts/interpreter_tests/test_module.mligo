
module F = struct
  let f (x : int) = x + 1
end

let main(_, i : unit * int) : operation list * int =
  ([] : operation list), F.f i

let test =
  let (taddr, _, _) = Test.originate main 0 0tez in
  let c = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn c () 0tez in
  Test.get_storage taddr
