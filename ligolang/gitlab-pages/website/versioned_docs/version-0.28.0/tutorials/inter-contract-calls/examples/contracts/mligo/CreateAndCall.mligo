// Here we create two operations: the one that will originate
// the contract, and an operation to self, that will continue
// the execution after the contract is originated.
let create_and_call (st : address list) =
  let create_op, addr =
    Tezos.create_contract
      (fun (p, s : int * int) -> ([] : operation list), p + s)
      (None : key_hash option)
      0tez
      1 in
  let call_op =
    Tezos.transaction
      (addr, 41)
      0tez
      (Tezos.self "%callback" : (address * int) contract)
  in [create_op; call_op], addr :: st

// At this point, we can be sure that the contract is originated
// already, so we forge an operation to call our counter contract.
// We need to check that the caller is self – just to make sure
// no-one is trying to abuse the %callback entrypoint to call
// other contracts on our behalf.
let call_counter (addr, n : address * int) =
  let u = assert (Tezos.sender = Tezos.self_address) in
  let callee_opt : int contract option =
    Tezos.get_contract_opt addr in
  let callee =
    match callee_opt with
    | Some contract -> contract
    | None -> (failwith "Could not find contract" : int contract)
  in Tezos.transaction n 0tez callee

type parameter =
  CreateAndCall
| Callback of address * int

let main (param, st : parameter * address list) =
  match param with
  | CreateAndCall -> create_and_call st
  | Callback vs -> [call_counter vs], st

