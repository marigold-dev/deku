open Crypto

module Withdrawal_handle : sig
  type t = private {
    hash : BLAKE2B.t;
    id : int;
    owner : Tezos.Address.t;
    amount : Amount.t;
    ticket : Ticket_id.t;
  }
  [@@deriving yojson]
end

type t [@@deriving yojson]

val empty : t

val ticket_table : t -> Ticket_table.t

val update_ticket_table : Ticket_table.t -> t -> t

val balance : Key_hash.t -> Ticket_id.t -> t -> Amount.t

val transfer :
  sender:Address.t ->
  destination:Key_hash.t ->
  Amount.t ->
  Ticket_id.t ->
  t ->
  (t, [> `Insufficient_funds]) result

val deposit : Key_hash.t -> Amount.t -> Ticket_id.t -> t -> t

val withdraw :
  sender:Key_hash.t ->
  destination:Tezos.Address.t ->
  Amount.t ->
  Ticket_id.t ->
  t ->
  (t * Withdrawal_handle.t, [> `Insufficient_funds]) result

val withdrawal_handles_find_proof :
  Withdrawal_handle.t -> t -> (BLAKE2B.t * BLAKE2B.t) list

val withdrawal_handles_find_proof_by_id :
  int -> t -> ((BLAKE2B.t * BLAKE2B.t) list * Withdrawal_handle.t) option

val withdrawal_handles_root_hash : t -> BLAKE2B.t
