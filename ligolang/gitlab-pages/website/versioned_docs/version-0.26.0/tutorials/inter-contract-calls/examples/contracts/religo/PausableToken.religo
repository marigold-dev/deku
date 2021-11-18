type parameter = SetPaused(bool) | Transfer((address, address, nat));

type storage = {ledger: big_map(address, nat), owner: address, paused: bool };

let lookup = ((addr, ledger): (address, big_map(address, nat))) => {
  switch(Map.find_opt(addr, ledger)){
  | Some (result) => result
  | None => 0n
  }
};

let transfer = ((src, dst, amount_, storage): (address, address, nat, storage)) => {
  let src_balance_opt = is_nat(lookup(src, storage.ledger) - amount_);
  switch(src_balance_opt){
  | Some (src_balance) =>
      {
        let dst_balance = lookup(dst, storage.ledger) + amount_;
        let ledger1 = Map.update(dst, Some (dst_balance), storage.ledger);
        let new_ledger = Map.update(src, Some (src_balance), ledger1);
        {...storage,
          ledger: new_ledger}
      }
  | None => (failwith("Insufficient funds") : storage)
  }
};

let main = ((p, s): (parameter, storage)) => {
  let nop: list(operation) = [];
  switch(p){
  | Transfer transfer_params =>
      if (s.paused) {
        (failwith("The contract is paused") : (list(operation), storage))
      } else {
        let (src, dst, amount_) = transfer_params;
        (nop, transfer(src, dst, amount_, s))
      }
  | SetPaused paused =>
      if (Tezos.sender != s.owner) {
        (failwith("Access denied") : (list(operation), storage))
      } else {
        (nop, {...s, paused: paused})
      }
  }
};
