type parameter is int

type storage is address

function get_add_entrypoint (const addr : address) is
block {
  const maybe_contract : option (contract (int)) = Tezos.get_contract_opt (addr)
} with
    case maybe_contract of [
      Some (contract) -> contract
    | None -> (failwith ("Callee does not exist") : contract (int))
    ]

function main (const param : parameter; const callee_addr : storage) is
block {
  const callee : contract (int) = get_contract (callee_addr);
  const op = Tezos.transaction (param, 0mutez, callee)
} with (list [op], callee_addr)
