open Deku_crypto
open Deku_concepts

module Withdrawal_handle : sig
  type t = {
    hash : BLAKE2b.t;
    id : int;
    owner : Deku_tezos.Address.t;
    amount : Amount.t;
    ticket_id : Ticket_id.t;
  }
  [@@deriving yojson]
end

type withdraw_proof [@@deriving yojson]
type withdraw_handle_tree

type ledger = private Ledger of { table : Ticket_table.t ; withdrawal_handles : withdraw_handle_tree }
type t = ledger

val with_ticket_table :
  t ->
  (get_table:(unit -> Ticket_table.t) -> set_table:(Ticket_table.t -> t) -> 'a) ->
  'a

val initial : t
val balance : Address.t -> Ticket_id.t -> t -> Amount.t
val deposit : Address.t -> Amount.t -> Ticket_id.t -> t -> t

val transfer :
  sender:Address.t ->
  receiver:Address.t ->
  amount:Amount.t ->
  ticket_id:Ticket_id.t ->
  t ->
  (t, [> `Insufficient_funds ]) result

val withdraw :
  sender:Address.t ->
  destination:Deku_tezos.Address.t ->
  amount:Amount.t ->
  ticket_id:Ticket_id.t ->
  t ->
  (t * Withdrawal_handle.t, [> `Insufficient_funds]) result

val withdrawal_handles_find_proof :
  Withdrawal_handle.t -> t -> withdraw_proof

val withdrawal_handles_find_proof_by_id :
  int -> t -> (withdraw_proof * Withdrawal_handle.t) option

val withdrawal_handles_root_hash : t -> BLAKE2b.t
