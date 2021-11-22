type parameter = Transfer of address * address * nat | SetPaused of bool

type storage = {ledger : (address, nat) big_map; owner : address; paused : bool}

let lookup (addr, ledger : address * (address, nat) big_map) : nat =
  match Big_map.find_opt addr ledger with
    Some result -> result
  | None -> 0n

let transfer (src, dst, amount_, storage : address * address * nat * storage) : storage =
  let src_balance_opt = Michelson.is_nat (lookup (src, storage.ledger) - amount_) in
  match src_balance_opt with
    None -> (failwith "Insufficient funds" : storage)
  | Some src_balance ->
      let dst_balance = lookup (dst, storage.ledger) + amount_ in
      let ledger1 = (Big_map.update dst (Some dst_balance) storage.ledger) in
      let new_ledger = Big_map.update src (Some src_balance) ledger1 in
      {storage with ledger = new_ledger}

let main (p, s : parameter * storage) =
  ([] : operation list),
  (match p with
     Transfer arg ->
       if s.paused
       then (failwith "The contract is paused" : storage)
       else
         let src, dst, amount_ = arg in
         transfer (src, dst, amount_, s)
   | SetPaused paused ->
       if Tezos.sender <> s.owner
       then (failwith "Access denied" : storage)
       else {s with paused = paused})
