open Deku_crypto
open Deku_concepts
open Deku_stdlib

module Withdrawal_handle = struct
  type t = {
    hash : BLAKE2b.t;
    id : int;
    owner : Deku_tezos.Address.t;
    amount : Amount.t;
    ticket_id : Ticket_id.t;
  }
  [@@deriving yojson]

  let equal handle1 handle2 = BLAKE2b.equal handle1.hash handle2.hash

  let hash ~id ~owner ~amount ~ticket_id =
    let (Ticket_id.Ticket_id { ticketer; data }) = ticket_id in
    let ticketer =
      Deku_tezos.Address.Originated { contract = ticketer; entrypoint = None }
    in
    Deku_tezos.Deku.Consensus.hash_withdraw_handle ~id:(Z.of_int id) ~owner
      ~amount:(N.to_z (Amount.to_n amount))
      ~ticketer ~data
end

module Withdrawal_handle_tree = Incremental_patricia.Make (struct
  type t = Withdrawal_handle.t [@@deriving yojson]

  let hash t = t.Withdrawal_handle.hash
end)

(* TODO: remove me? *)
type withdraw_handle_tree = Withdrawal_handle_tree.t

type ledger =
  | Ledger of {
      table : Ticket_table.t;
      withdrawal_handles : withdraw_handle_tree;
    }

and t = ledger

type withdraw_proof = (BLAKE2b.t * BLAKE2b.t) list [@@deriving yojson]
type withdraw_handle = Withdrawal_handle.t [@@deriving yojson]

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
    |> Result.map_error (fun _ -> `Insufficient_funds)
  in
  Ok (Ledger { table; withdrawal_handles })

let withdraw ~sender ~destination ~amount ~ticket_id t =
  let (Ledger { table; withdrawal_handles }) = t in
  let%ok table =
    Ticket_table.withdraw ~sender ~amount ~ticket_id table
    |> Result.map_error (function _ -> `Insufficient_funds)
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

(*
let total_balance ~ticket_id (Ledger { table; withdrawal_handles = _ }) =
  let open Ticket_table in
  Address_map.fold
    (fun _sender ticket_map total_amount ->
      Ticket_map.find_opt ticket_id ticket_map
      |> Option.value ~default:Amount.zero
      |> Amount.( + ) total_amount)
    (table :> Amount.t Ticket_map.t Address_map.t)
    Amount.zero
*)

let show_ledger ledger =
  let open Ticket_table in
  let (Ledger { table; _ }) = ledger in
  Address_map.iter
    (fun address map ->
      Printf.printf "** %s\n" (Address.to_b58 address);
      Ticket_map.iter
        (fun ticket_id amount ->
          Printf.printf "    %s: %d\n%!"
            (Ticket_id.yojson_of_ticket_id ticket_id |> Yojson.Safe.to_string)
            (amount |> Amount.to_n |> Deku_stdlib.N.to_z |> Z.to_int))
        map)
    (table :> Amount.t Ticket_map.t Address_map.t)
