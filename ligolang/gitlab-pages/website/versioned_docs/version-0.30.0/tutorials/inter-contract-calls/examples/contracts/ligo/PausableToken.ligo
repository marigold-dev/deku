type parameter is SetPaused of bool | Transfer of address * address * nat

type storage is
  record [ledger : big_map (address, nat); owner : address; paused : bool]

function lookup (const addr : address; const ledger : big_map (address, nat)) is
  case Map.find_opt (addr, ledger) of [
    Some (result) -> result
  | None -> 0n
  ]

function transfer
  (const src : address;
   const dst : address;
   const amount_ : nat;
   const storage : storage) is
block {
  const src_balance_opt = is_nat (lookup (src, storage.ledger) - amount_)
} with
    case src_balance_opt of [
      Some (src_balance) ->
        block {
          const dst_balance = lookup (dst, storage.ledger) + amount_;
          const ledger1 = Map.update (dst, Some (dst_balance), storage.ledger);
          const new_ledger = Map.update (src, Some (src_balance), ledger1)
        } with storage with record [ledger = new_ledger]
    | None -> (failwith ("Insufficient funds") : storage)
    ]

function main (const p : parameter; const s : storage) is
block {
  const nop : list (operation) = list []
} with
    case p of [
      Transfer (arg) ->
        if s.paused
        then
          (failwith ("The contract is paused") : (list (operation) * storage))
        else
          block {
            const src = arg.0;
            const dst = arg.1;
            const amount_ = arg.2
          } with (nop, transfer (src, dst, amount_, s))
    | SetPaused (paused) ->
        if (Tezos.sender =/= s.owner)
        then (failwith ("Access denied") : (list (operation) * storage))
        else (nop, s with record [paused = paused])
