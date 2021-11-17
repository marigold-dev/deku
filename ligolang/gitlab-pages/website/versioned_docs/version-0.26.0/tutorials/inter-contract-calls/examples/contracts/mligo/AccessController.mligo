(* We show this contract to demonstrate a possible flaw in
   inter-contract communications. Do not use it in production:
   it's vulnerable because it contains a view entrypoint.
   You should either remove this entrypoint or use tickets
   for authorization. See "Inter-contract invocations" and
   "Security" tutorials for the details
*)

type parameter =
  Call of unit -> operation | IsWhitelisted of address * (bool contract)

type storage = {senders_whitelist : address set}

let main (p, s : parameter * storage) =
  let op =
    match p with
      Call op ->
        if Set.mem Tezos.sender s.senders_whitelist
        then op ()
        else (failwith "Sender is not whitelisted" : operation)
    | IsWhitelisted arg ->
        let addr, callback_contract = arg in
        let whitelisted = Set.mem addr s.senders_whitelist in
        Tezos.transaction whitelisted 0mutez callback_contract in
  [op], s
