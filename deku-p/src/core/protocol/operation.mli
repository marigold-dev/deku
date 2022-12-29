open Deku_crypto
open Deku_concepts
open Deku_ledger

exception Invalid_signature
exception Invalid_source

type operation =
  | Operation_ticket_transfer of {
      sender : Address.t;
      receiver : Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_attest_twitch_handle of {
      sender : Address.t;
      twitch_handle : string;
    }
  | Operation_attest_deku_address of {
      sender : Address.t;
      deku_address : Address.t;
      twitch_handle : string;
    }
  | Operation_vote of { sender : Address.t; vote : Game.Vote.t }
  | Operation_delegated_vote of {
      sender : Address.t;
      twitch_handle : Game.Twitch_handle.t;
      vote : Game.Vote.t;
    }
  | Operation_withdraw of {
      sender : Address.t;
      owner : Deku_tezos.Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_noop of { sender : Address.t }

type t = operation [@@deriving show]

val encoding : t Data_encoding.t

module Initial : sig
  type initial_operation = private
    | Initial_operation of {
        hash : Operation_hash.t;
        nonce : Nonce.t;
        level : Level.t;
        operation : operation;
      }

  type t = initial_operation [@@deriving show]
  type hash_repr = Nonce.t * Level.t * operation

  val hash_encoding : (Nonce.t * Level.t * operation) Data_encoding.t
  val encoding : t Data_encoding.t

  (* helpers *)
  val last_includable_level : initial_operation -> Level.t

  val is_in_includable_window :
    current_level:Level.t -> operation_level:Level.t -> bool
end

module Signed : sig
  type signed_operation = private
    | Signed_operation of {
        key : Key.t;
        signature : Signature.t;
        initial : Initial.t;
      }

  type t = signed_operation [@@deriving show]

  val encoding : signed_operation Data_encoding.t

  val make_with_signature :
    key:Key.t ->
    signature:Signature.t ->
    initial:Initial.t ->
    signed_operation option

  val ticket_transfer :
    identity:Identity.t ->
    nonce:Nonce.t ->
    level:Level.t ->
    receiver:Address.t ->
    ticket_id:Ticket_id.t ->
    amount:Amount.t ->
    signed_operation

  val withdraw :
    identity:Identity.t ->
    nonce:Nonce.t ->
    level:Level.t ->
    tezos_owner:Deku_tezos.Address.t ->
    ticket_id:Ticket_id.t ->
    amount:Amount.t ->
    signed_operation

  val noop :
    identity:Identity.t -> nonce:Nonce.t -> level:Level.t -> signed_operation

  val vote :
    nonce:Nonce.t ->
    level:Level.t ->
    vote:Game.Vote.t ->
    identity:Identity.t ->
    signed_operation

  val delegated_vote :
    nonce:Nonce.t ->
    level:Level.t ->
    vote:Game.Vote.t ->
    twitch_handle:string ->
    identity:Identity.t ->
    signed_operation

  val attest_twitch_handle :
    nonce:Nonce.t ->
    level:Level.t ->
    twitch_handle:string ->
    identity:Identity.t ->
    signed_operation

  val attest_deku_address :
    nonce:Nonce.t ->
    level:Level.t ->
    deku_address:Address.t ->
    twitch_handle:string ->
    identity:Identity.t ->
    signed_operation
end
