open Deku_concepts
open Deku_crypto
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

(* FIXME: placeholder for the real implementation of withdrawal handle hash.
   Right now we only need it to commit on Tezos. *)
module Withdrawal_handle_hash = struct
  type t = BLAKE2b.t

  include Deku_repr.With_b58_and_yojson (struct
    type nonrec t = t

    let prefix = Deku_repr.Prefix.deku_withdrawal_hash
    let to_raw = BLAKE2b.to_raw
    let of_raw = BLAKE2b.of_raw
  end)
end
