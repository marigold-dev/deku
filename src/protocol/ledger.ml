open Deku_concepts
open Amount

type ledger = Amount.t Address.Map.t
type t = ledger

let initial = Address.Map.empty

let balance address ledger =
  match Address.Map.find_opt address ledger with
  | Some balance -> balance
  | None -> Amount.zero

let deposit address amount ledger =
  let balance = balance address ledger in
  let balance = balance + amount in
  Address.Map.add address balance ledger

let transfer ~sender ~receiver amount ledger =
  let sender_balance = balance sender ledger in
  let receiver_balance = balance receiver ledger in

  match sender_balance - amount with
  | Some sender_balance ->
      let receiver_balance = receiver_balance + amount in
      let ledger = Address.Map.add sender sender_balance ledger in
      let ledger = Address.Map.add receiver receiver_balance ledger in
      Some ledger
  | None -> None
