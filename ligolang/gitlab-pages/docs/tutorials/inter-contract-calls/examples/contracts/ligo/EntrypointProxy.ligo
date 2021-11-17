type parameter is int

type storage is address

function get_add_entrypoint (const addr : address) is
block {
  const entrypoint : option (contract (int))
  = Tezos.get_entrypoint_opt ("%add", addr)
} with
    case entrypoint of [
      Some (contract) -> contract
    | None -> (failwith ("The entrypoint does not exist") : contract (int))
    ]

function main (const param : parameter; const callee_addr : storage) is
block {
  const add : contract (int) = get_add_entrypoint (callee_addr);
  const op = Tezos.transaction (param, 0mutez, add)
} with (list [op], callee_addr)
