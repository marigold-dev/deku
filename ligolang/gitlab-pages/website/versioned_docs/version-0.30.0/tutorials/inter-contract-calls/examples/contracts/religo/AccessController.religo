/* We show this contract to demonstrate a possible flaw in
   inter-contract communications. Do not use it in production:
   it's vulnerable because it contains a view entrypoint.
   You should either remove this entrypoint or use tickets
   for authorization. See "Inter-contract invocations" and
   "Security" tutorials for the details
*/

type parameter =
  Call((unit => operation))
| IsWhitelisted((address, contract(bool)));

type storage = {senders_whitelist: set(address) };

let main = ((p, s): (parameter, storage)) => {
  let op = 
    switch(p){
    | Call op =>
        {
          if (Set.mem(Tezos.sender, s.senders_whitelist)) {
            op()
          } else {
            (failwith("Sender is not whitelisted") : operation)
          }
        }
    | IsWhitelisted addr_and_callback =>
        {
          let addr = addr_and_callback[0];
          let callback_contract = addr_and_callback[1];
          let whitelisted = Set.mem(addr, s.senders_whitelist);
          Tezos.transaction(whitelisted, 0mutez, callback_contract)
        }
    };
  ([op], s)
};
