type parameter = int

type storage = address

let get_contract (addr : address) =
  match (Tezos.get_contract_opt addr : int contract option) with
    Some contract -> contract
  | None -> (failwith "Callee does not exist" : int contract)

let main (param, callee_addr : parameter * storage) =
  let callee : int contract = get_contract (callee_addr) in
  let op = Tezos.transaction param 0mutez callee in
  [op], callee_addr
