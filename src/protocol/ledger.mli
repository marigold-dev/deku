open Deku_crypto
open Deku_concepts

module Withdrawal_handle : sig
  module Withdrawal_handle_hash : sig
    type t = BLAKE2b.t [@@deriving yojson]
  end

  type hash = Withdrawal_handle_hash.t [@@deriving yojson]

  type t = {
    hash : Withdrawal_handle_hash.t;
    id : int;
    owner : Deku_tezos.Address.t;
    amount : Amount.t;
    ticket_id : Ticket_id.t;
  }
  [@@deriving yojson, eq]
end

type withdraw_proof = (Withdrawal_handle.hash * Withdrawal_handle.hash) list
[@@deriving yojson]

module Proof_response : sig
  type t =
    | Proof of {
        operation_hash : Operation_hash.t;
        handle : Withdrawal_handle.t;
        proof : withdraw_proof;
      }
  [@@deriving yojson]
end

type withdraw_handle_tree
type withdraw_handle = Withdrawal_handle.t [@@deriving yojson]

type ledger = private
  | Ledger of {
      table : Ticket_table.t;
      withdrawal_handles : withdraw_handle_tree;
    }

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
  (t * Withdrawal_handle.t, [> `Insufficient_funds ]) result

val withdrawal_handles_find_proof : Withdrawal_handle.t -> t -> withdraw_proof

val withdrawal_handles_find_proof_by_id :
  int -> t -> (withdraw_proof * Withdrawal_handle.t) option

val withdrawal_handles_root_hash : t -> BLAKE2b.t
