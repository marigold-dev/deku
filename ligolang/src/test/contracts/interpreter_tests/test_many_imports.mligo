#import "C.mligo" "C"

let test =
  let (taddr, _, _) = Test.originate C.main () 0tez in
  let c = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn c () 0tez in
  assert (Test.get_storage taddr = ())
