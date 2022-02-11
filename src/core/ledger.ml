open Helpers
open Crypto
module Address_and_ticket_map = struct
  type key = {
    address : Key_hash.t;
    ticket : Ticket_id.t;
  }
  [@@deriving ord, yojson]
  module Map = Map.Make_with_yojson (struct
    type t = key [@@deriving ord, yojson]
  end)
  type t = Ticket_table.handle Map.t [@@deriving yojson]
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
  (*
     The ticket_id is duplicated in ledger and ticket_table
     for fast lookup.
     So it's important that we make sure that
     ledger[ticket_table[{ticket_id, _}]] = {ticket_id', _}
     implies ticket_id = ticket_id'.
  *)
  ledger : Address_and_ticket_map.t;
  ticket_table : Ticket_table.t;
  withdrawal_handles : Withdrawal_handle_tree.t;
}
[@@deriving yojson]
let empty =
  {
    ledger = Address_and_ticket_map.empty;
    ticket_table = Ticket_table.empty;
    withdrawal_handles = Withdrawal_handle_tree.empty;
  }
let balance address ticket t =
  let balance =
    let%some handle = Address_and_ticket_map.find_opt address ticket t.ledger in
    let%some ticket = Ticket_table.find_opt handle t.ticket_table in
    Some ticket.amount in
  balance |> Option.value ~default:Amount.zero

let get_or_create_ticket_handle address ticket ticket_table ledger =
  match Address_and_ticket_map.find_opt address ticket ledger with
  | None ->
    let handle, ticket_table = Ticket_table.add_empty ticket ticket_table in
    let ledger = Address_and_ticket_map.add address ticket handle ledger in
    (handle, ticket_table, ledger)
  | Some handle -> (handle, ticket_table, ledger)

let transfer ~sender ~destination amount ticket t =
  let%ok sender_handle =
    Address_and_ticket_map.find_opt sender ticket t.ledger
    |> Option.to_result ~none:`Not_enough_funds in
  let destination_handle, ticket_table, ledger =
    get_or_create_ticket_handle destination ticket t.ticket_table t.ledger in
  let%ok Ticket_table.{ split_at; remaining = sender_handle }, ticket_table =
    Ticket_table.split sender_handle ~at:amount ticket_table
    |> Result.map_error (function
         | `Invalid_ticket -> failwith "Should be impossible"
         | `Not_enough_funds as e -> e) in
  let destination_handle, ticket_table =
    Ticket_table.join destination_handle split_at ticket_table |> Result.get_ok
  in
  Ok
    {
      ledger =
        ledger
        |> Address_and_ticket_map.add sender ticket sender_handle
        |> Address_and_ticket_map.add destination ticket destination_handle;
      ticket_table;
      withdrawal_handles = t.withdrawal_handles;
    }

let deposit destination amount ticket t =
  let to_merge_handle, ticket_table =
    Ticket_table.add
      (Ticket_table.unsafe_create_ticket ticket amount)
      t.ticket_table in
  let destination_handle, ticket_table, ledger =
    get_or_create_ticket_handle destination ticket ticket_table t.ledger in
  let destination_handle, ticket_table =
    Ticket_table.join destination_handle to_merge_handle ticket_table
    |> Result.get_ok in
  {
    ledger =
      ledger |> Address_and_ticket_map.add destination ticket destination_handle;
    ticket_table;
    withdrawal_handles = t.withdrawal_handles;
  }
let withdraw ~sender ~destination amount ticket t =
  let owner = destination in
  let%ok sender_handle =
    Address_and_ticket_map.find_opt sender ticket t.ledger
    |> Option.to_result ~none:`Not_enough_funds in
  let%ok Ticket_table.{ split_at; remaining }, ticket_table =
    Ticket_table.split sender_handle ~at:amount t.ticket_table
    |> Result.map_error (function
         | `Invalid_ticket -> failwith "Should be impossible"
         | `Not_enough_funds as e -> e) in
  let Ticket_table.{ id = _; amount = amount_withdrawn }, ticket_table =
    Ticket_table.remove split_at ticket_table |> Result.get_ok in
  let withdrawal_handles, withdrawal_handle =
    Withdrawal_handle_tree.add
      (fun id ->
        let hash = Withdrawal_handle.hash ~id ~owner ~amount ~ticket in
        { id; hash; owner; amount = amount_withdrawn; ticket })
      t.withdrawal_handles in
  let t =
    {
      ledger = t.ledger |> Address_and_ticket_map.add sender ticket remaining;
      ticket_table;
      withdrawal_handles;
    } in
  Ok (t, withdrawal_handle)
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
