type storage = {
  challenge : string
}

type param = {
  new_challenge : string;
  attempt       : string
}

type return = operation list * storage

let attempt (p, store : param * storage) : return =
  (* if p.attempt <> store.challenge then failwith "Failed challenge" else *)
  let contract : unit contract =
    match (Tezos.get_contract_opt Tezos.sender : unit contract option) with
      Some contract -> contract
    | None ->  (failwith "No contract" : unit contract)
  in
  let transfer : operation =
    Tezos.transaction (unit, contract, 10.00tez) in
  let store : storage = {challenge = p.new_challenge}
  in ([] : operation list), store
