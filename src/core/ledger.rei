open Crypto
module Handle :
sig
  type t = private
    {
    hash: BLAKE2B.t ;
    id: int ;
    owner: Tezos.Address.t ;
    amount: Amount.t ;
    ticket: Ticket_id.t }[@@deriving yojson]
end
type t[@@deriving yojson]
val empty : t
val balance : Key_hash.t -> Ticket_id.t -> t -> Amount.t
val transfer :
  sender:Key_hash.t ->
    destination:Key_hash.t ->
      Amount.t -> Ticket_id.t -> t -> (t, [> `Not_enough_funds ]) result
val deposit : Key_hash.t -> Amount.t -> Ticket_id.t -> t -> t
val withdraw :
  sender:Key_hash.t ->
    destination:Tezos.Address.t ->
      Amount.t ->
        Ticket_id.t -> t -> ((t * Handle.t), [> `Not_enough_funds ]) result
val handles_find_proof : Handle.t -> t -> (BLAKE2B.t * BLAKE2B.t) list
val handles_find_proof_by_id :
  int -> t -> ((BLAKE2B.t * BLAKE2B.t) list * Handle.t) option
val handles_root_hash : t -> BLAKE2B.t