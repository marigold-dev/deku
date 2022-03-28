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
val balance : Address.t -> Ticket_id.t -> t -> Amount.t
val transfer :
  sender:Address.t ->
  destination:Address.t ->
  Amount.t ->
  Ticket_id.t ->
  t ->
  (t, [> `Not_enough_funds]) result
val deposit : Address.t -> Amount.t -> Ticket_id.t -> t -> t
val burn : t -> sender:Address.t -> ticket:Ticket_id.t -> amount:Amount.t -> t
val withdraw :
  sender:Address.t ->
  destination:Tezos.Address.t ->
  Amount.t ->
  Ticket_id.t ->
  t ->
  (t * Withdrawal_handle.t, [> `Not_enough_funds]) result
val withdrawal_handles_find_proof :
  Withdrawal_handle.t -> t -> (BLAKE2B.t * BLAKE2B.t) list
val withdrawal_handles_find_proof_by_id :
  int -> t -> ((BLAKE2B.t * BLAKE2B.t) list * Withdrawal_handle.t) option
val withdrawal_handles_root_hash : t -> BLAKE2B.t
