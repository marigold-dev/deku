open Helpers
open Crypto
module Address_and_ticket_map = struct
  type key = {
    address : Address.t;
    ticket : Ticket_id.t;
  }
  [@@deriving ord, yojson]
  module Map = Map.Make_with_yojson (struct
    type t = key [@@deriving ord, yojson]
  end)
  type t = Amount.t Map.t [@@deriving yojson]
  let empty = Map.empty
  let find_opt address ticket = Map.find_opt { address; ticket }
  let add address ticket = Map.add { address; ticket }
end
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
  ledger : Address_and_ticket_map.t;
  withdrawal_handles : Withdrawal_handle_tree.t;
}
[@@deriving yojson]
let empty =
  {
    ledger = Address_and_ticket_map.empty;
    withdrawal_handles = Withdrawal_handle_tree.empty;
  }
let balance address ticket t =
  Address_and_ticket_map.find_opt address ticket t.ledger
  |> Option.value ~default:Amount.zero
let assert_available ~sender ~(amount : Amount.t) =
  if sender >= amount then
    Ok ()
  else
    Error `Not_enough_funds
let transfer ~sender ~destination amount ticket t =
  let open Amount in
  let sender_balance = balance sender ticket t in
  let%ok () = assert_available ~sender:sender_balance ~amount in
  let destination_balance = balance destination ticket t in
  Ok
    {
      ledger =
        t.ledger
        |> Address_and_ticket_map.add sender ticket (sender_balance - amount)
        |> Address_and_ticket_map.add destination ticket
             (destination_balance + amount);
      withdrawal_handles = t.withdrawal_handles;
    }
let deposit destination amount ticket t =
  let open Amount in
  let destination_balance = balance destination ticket t in
  {
    ledger =
      t.ledger
      |> Address_and_ticket_map.add destination ticket
           (destination_balance + amount);
    withdrawal_handles = t.withdrawal_handles;
  }

(* TODO: proper semantics ???  *)
let burn t ~sender ~ticket ~amount =
  let open Amount in
  let sender_balance = balance sender ticket t in
  {
    t with
    ledger =
      t.ledger
      |> Address_and_ticket_map.add sender ticket (sender_balance - amount);
  }

let withdraw ~sender ~destination amount ticket t =
  let open Amount in
  let owner = destination in
  let sender_balance = balance sender ticket t in
  let%ok () = assert_available ~sender:sender_balance ~amount in
  let withdrawal_handles, handle =
    Withdrawal_handle_tree.add
      (fun id ->
        let hash = Withdrawal_handle.hash ~id ~owner ~amount ~ticket in
        { id; hash; owner; amount; ticket })
      t.withdrawal_handles in
  let t =
    {
      ledger =
        t.ledger
        |> Address_and_ticket_map.add sender ticket (sender_balance - amount);
      withdrawal_handles;
    } in
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
