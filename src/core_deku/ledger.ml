open Helpers
open Crypto

module Withdrawal_handle = struct
  type t = {
    hash : BLAKE2B.t;
    id : int;
    owner : Tezos.Address.t;
    amount : Amount.t;
    ticket : Ticket_id.t;
  }
  [@@deriving yojson]
  let hash ~id ~owner ~amount ~ticket =
    let Ticket_id.{ ticketer; data } = ticket in
    Tezos.Deku.Consensus.hash_withdraw_handle ~id:(Z.of_int id) ~owner
      ~amount:(Z.of_int (Amount.to_int amount))
      ~ticketer ~data
end
module Withdrawal_handle_tree = Incremental_patricia.Make (struct
  type t = Withdrawal_handle.t [@@deriving yojson]
  let hash t = t.Withdrawal_handle.hash
end)
type t = {
  ledger : Ticket_table.t;
  withdrawal_handles : Withdrawal_handle_tree.t;
}
[@@deriving yojson]
let empty =
  {
    ledger = Ticket_table.empty;
    withdrawal_handles = Withdrawal_handle_tree.empty;
  }
let balance address ticket t =
  Ticket_table.amount t.ledger (Address.of_key_hash address) ticket
  |> Option.value ~default:Amount.zero

let transfer ~sender ~destination amount ticket t =
  let%ok ledger =
    Ticket_table.transfer t.ledger
      ~sender:(Address.of_key_hash sender)
      ~destination:(Address.of_key_hash destination)
      ~amount ~ticket
    |> Result.map_error (fun _ -> `Insufficient_funds) in
  Ok { ledger; withdrawal_handles = t.withdrawal_handles }
let deposit destination amount ticket t =
  let ledger =
    Ticket_table.unsafe_deposit_ticket t.ledger ~ticket
      ~destination:(Address.of_key_hash destination)
      ~amount in
  { ledger; withdrawal_handles = t.withdrawal_handles }

let withdraw ~sender ~destination amount ticket t =
  let%ok ledger =
    Ticket_table.unsafe_withdraw t.ledger
      ~sender:(Address.of_key_hash sender)
      ~amount ~ticket
    |> Result.map_error (function _ -> `Insufficient_funds) in
  let withdrawal_handles, handle =
    Withdrawal_handle_tree.add
      (fun id ->
        let hash =
          Withdrawal_handle.hash ~id ~owner:destination ~amount ~ticket in
        { id; hash; owner = destination; amount; ticket })
      t.withdrawal_handles in
  let t = { ledger; withdrawal_handles } in
  Ok (t, handle)
let withdrawal_handles_find_proof handle t =
  match
    Withdrawal_handle_tree.find handle.Withdrawal_handle.id t.withdrawal_handles
  with
  | None -> assert false
  | Some (proof, _) -> proof
let withdrawal_handles_find_proof_by_id key t =
  Withdrawal_handle_tree.find key t.withdrawal_handles
let withdrawal_handles_root_hash t =
  Withdrawal_handle_tree.hash t.withdrawal_handles
