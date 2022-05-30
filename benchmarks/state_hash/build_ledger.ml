(*
1. Build type `key`:

  type key =
  {
      address: Key_hash.t;
      ticket: Ticket_id.t;
  } 

1.1 Build `address: Key_hash.t`: make_address ()

1.2  Build `ticket: Ticket_id.t`: make_ticket ()
  
  src/core_deku/ticket_id.ml:

  type t = Tezos.Ticket_id.t =
   {
     ticketer : Tezos.Address.t;
     data: bytes
   }
*)

open Ledger
open Crypto

let make_address () =
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  key_hash

let make_ticket ?ticketer ?data () =
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Random.generate 20
        |> Cstruct.to_string
        |> BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  { ticketer; data }

(* a. Build `ledger: Address_and_ticket_map.t` type with transfer and deposit

   - Address_and_ticket_map.add address ticket
*)

let setup_deposit destination amount ticket_id () =
  let ledger = empty |> deposit destination amount ticket_id in
  ledger

let setup_transfer sender destination amount ticket_id ledger =
  let ledger = transfer ~sender ~destination amount ticket_id ledger in
  ledger

(* b. Build `withdrawal_handles: Withdrawal_handle_tree.t` with withdraw function *)

(* The address of a destination of a withdraw must be a tezos_address *)
let make_tezos_address () =
  let open Tezos in
  let _key, address = Ed25519.generate () in
  let hash = Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

let setup_withdraw sender destination amount ticket_id ledger () =
  let ledger = withdraw ~sender ~destination amount ticket_id ledger in
  ledger
