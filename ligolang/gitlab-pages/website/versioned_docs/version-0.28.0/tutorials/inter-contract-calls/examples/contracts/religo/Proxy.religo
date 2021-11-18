type parameter = int;

type storage = address;

let get_contract = (addr: address) => {
  let maybe_contract: option(contract(int)) = Tezos.get_contract_opt(addr);
  switch(maybe_contract){
  | Some (contract) => contract
  | None => (failwith("Callee does not exist") : contract(int))
  }
};

let main = ((param, callee_addr): (parameter, storage)) => {
  let callee: contract(int) = get_contract(callee_addr);
  let op = Tezos.transaction(param, 0mutez, callee);
  ([op], callee_addr)
};
