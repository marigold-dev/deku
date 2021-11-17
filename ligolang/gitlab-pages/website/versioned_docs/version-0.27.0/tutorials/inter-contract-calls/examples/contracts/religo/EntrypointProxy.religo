type parameter = int;

type storage = address;

let get_add_entrypoint = (addr: address) => {
  let entrypoint: option(contract(int)) = Tezos.get_entrypoint_opt("%add", addr);
  switch(entrypoint){
  | Some (contract) => contract
  | None => (failwith("The entrypoint does not exist") : contract(int))
  }
};

let main = ((param, callee_addr): (parameter, storage)) => {
  let add: contract(int) = get_add_entrypoint(callee_addr);
  let op = Tezos.transaction(param, 0mutez, add);
  ([op], callee_addr)
};
