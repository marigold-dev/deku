let create_and_call =  (st: list(address)) => {
    let (create_op, addr) =
        Tezos.create_contract(
            ((p, s): (int, int)) => ([] : list(operation), (p + s)),
            (None : option(key_hash)),
            0mutez,
            1
        );
    let call_op =
        Tezos.transaction(
            (addr, 41),
            0mutez,
            Tezos.self("%callback") : contract((address, int))
        );
    ([create_op, call_op], [addr, ...st]);
};

let call_counter = ((addr, n): (address, int)) => {
    let u = assert(Tezos.sender == Tezos.self_address);
    let callee_opt: option(contract(int)) = 
        Tezos.get_contract_opt(addr);
    let callee = 
        switch(callee_opt) {
        | Some(contract) => contract
        | None =>
            (failwith("Could not find contract") : contract(int))
        };
    Tezos.transaction(n, 0mutez, callee);
};

type parameter = 
| Callback((address, int))
| CreateAndCall;

let main = ((param, st): (parameter, list(address))) => {
    switch(param) {
    | CreateAndCall => create_and_call(st)
    | Callback vs => ([call_counter(vs)], st)
    };
}
