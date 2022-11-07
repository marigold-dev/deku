open Deku_crypto
open Deku_concepts
open Deku_stdlib

exception Insufficient_funds
exception Withdraw_zero_ticket

let () =
  Printexc.register_printer (function
    | Insufficient_funds -> Some "Ledger error: insufficient funds"
    | Withdraw_zero_ticket -> Some "Ledger error: cannot withdraw 0 tickets"
    | _ -> None)

module Withdrawal_handle = struct
  module Withdrawal_handle_hash = struct
    open BLAKE2b

    type t = BLAKE2b.t [@@deriving show]

    include With_b58_and_encoding_and_yojson (struct
      let name = "Ledger.Withdrawal_handle_hash"
      let prefix = Deku_repr.Prefix.deku_withdrawal_hash
    end)
  end

  type hash = Withdrawal_handle_hash.t [@@deriving yojson, show]

  type t = {
    hash : Withdrawal_handle_hash.t;
    id : int;
    owner : Deku_tezos.Address.t;
    amount : Amount.t;
    ticket_id : Ticket_id.t;
  }
  [@@deriving yojson]

  let equal handle1 handle2 = BLAKE2b.equal handle1.hash handle2.hash

  let hash ~id ~owner ~amount ~ticket_id =
    let[@warning "-8"] (Ticket_id.Ticket_id { ticketer = Tezos ticketer; data })
        =
      ticket_id
    in
    let ticketer =
      Deku_tezos.Address.Originated { contract = ticketer; entrypoint = None }
    in
    Deku_tezos.Deku.Consensus.hash_withdrawal_handle ~id:(Z.of_int id) ~owner
      ~amount:(N.to_z (Amount.to_n amount))
      ~ticketer ~data
end

module Withdrawal_handle_tree = Incremental_patricia.Make (struct
  type t = Withdrawal_handle.t [@@deriving yojson]

  let hash t = t.Withdrawal_handle.hash
end)

type withdraw_proof = (Withdrawal_handle.hash * Withdrawal_handle.hash) list
[@@deriving yojson]

module Proof_response = struct
  type t =
    | Proof of {
        withdrawal_handles_hash : Withdrawal_handle.hash;
        handle : Withdrawal_handle.t;
        proof : withdraw_proof;
      }
  [@@deriving yojson]
end

type ledger =
  | Ledger of {
      table : Ticket_table.t;
      withdrawal_handles : Withdrawal_handle_tree.t;
    }
[@@deriving yojson]

and t = ledger

type withdrawal_handle = Withdrawal_handle.t [@@deriving yojson]

let initial =
  Ledger
    {
      table = Ticket_table.empty;
      withdrawal_handles = Withdrawal_handle_tree.empty;
    }

let with_ticket_table t f =
  let (Ledger { table; withdrawal_handles }) = t in
  f ~get_table:(Fun.const table) ~set_table:(fun table ->
      Ledger { table; withdrawal_handles })

let balance address ticket_id (Ledger { table; _ }) =
  Ticket_table.balance ~sender:address ~ticket_id table
  |> Option.value ~default:Amount.zero

let deposit destination amount ticket_id (Ledger { table; withdrawal_handles })
    =
  let table = Ticket_table.deposit ~destination ~ticket_id ~amount table in
  Ledger { table; withdrawal_handles }

let transfer ~sender ~receiver ~amount ~ticket_id
    (Ledger { table; withdrawal_handles }) =
  let%ok table =
    Ticket_table.transfer table ~sender ~receiver ~amount ~ticket_id
    |> Result.map_error (fun _ -> Insufficient_funds)
  in
  Ok (Ledger { table; withdrawal_handles })

let withdraw ~sender ~destination ~amount ~ticket_id t =
  let (Ledger { table; withdrawal_handles }) = t in
  let%ok ticket_id =
    match ticket_id with
    | Ticket_id.Ticket_id { ticketer = Deku _; data = _ } ->
        Error Insufficient_funds
    | Ticket_id.Ticket_id { ticketer = Tezos _; data = _ } -> Ok ticket_id
  in
  let%ok table =
    Ticket_table.withdraw ~sender ~amount ~ticket_id table
    |> Result.map_error (function error ->
           (match error with
           | `Withdraw_zero_ticket -> Withdraw_zero_ticket
           | `Insufficient_funds -> Insufficient_funds))
  in
  let withdrawal_handles, handle =
    Withdrawal_handle_tree.add
      (fun id ->
        let hash =
          Withdrawal_handle.hash ~id ~owner:destination ~amount ~ticket_id
        in
        { id; hash; owner = destination; amount; ticket_id })
      withdrawal_handles
  in
  let t = Ledger { table; withdrawal_handles } in
  Ok (t, handle)

let withdrawal_handles_find_proof handle t =
  let (Ledger { withdrawal_handles; _ }) = t in
  match
    Withdrawal_handle_tree.find handle.Withdrawal_handle.id withdrawal_handles
  with
  | None -> assert false
  | Some (proof, _) -> proof

let withdrawal_handles_find_proof_by_id key (Ledger { withdrawal_handles; _ }) =
  Withdrawal_handle_tree.find key withdrawal_handles

let withdrawal_handles_root_hash (Ledger { withdrawal_handles; _ }) =
  Withdrawal_handle_tree.hash withdrawal_handles
