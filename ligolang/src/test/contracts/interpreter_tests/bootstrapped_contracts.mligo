
type storage =
  {
    fellow : address;
    state : int;
  }

let main (_, storage : unit * storage) : operation list * storage =
  if storage.state > 0 then
    (let contract = match (Tezos.get_contract_opt storage.fellow : unit contract option) with
       | Some contract -> contract
       | None -> (failwith "oops" : unit contract) in
     let op = Tezos.transaction () 0mutez contract in
     let storage = { storage with state = storage.state - 1 } in
     (([op]: operation list), storage))
  else
    (([] : operation list), storage)

let addr0 = Test.nth_bootstrap_contract 0n
let addr1 = Test.nth_bootstrap_contract 1n

(* Init contract in addr0 pointing to addr1 *)
let u = Test.bootstrap_contract 100mutez main { fellow = addr1; state = 12; }
(* Init contract in addr1 pointing to addr0 *)
let u = Test.bootstrap_contract 100mutez main { fellow = addr0; state = 9; }

let u = Test.reset_state 4n ([] : tez list)

let test_transfer =
  let () = Test.log "Initial states:" in
  let storage = Test.get_storage_of_address addr0 in
  let () = Test.log storage in
  let storage = Test.get_storage_of_address addr1 in
  let () = Test.log storage in
  let _ = Test.transfer_exn addr1 (Test.eval ()) 1tez in
  let () = Test.log "Final states:" in
  let storage = Test.get_storage_of_address addr0 in
  let () = Test.log storage in
  let storage = Test.get_storage_of_address addr1 in
  let () = Test.log storage in
  ()
