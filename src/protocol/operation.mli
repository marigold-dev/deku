open Deku_crypto
open Deku_concepts

exception Invalid_signature

type operation_content = private
  | Operation_ticket_transfer of { receiver : Address.t; amount : Amount.t }
  | Operation_vm_transaction of { operation : string; tickets : Ticket.t list }
  | Operation_noop

type operation = private
  | Operation of {
      key : Key.t;
      signature : Signature.t;
      hash : Operation_hash.t;
      level : Level.t;
      nonce : Nonce.t;
      source : Address.t;
      content : operation_content;
    }

type t = operation [@@deriving eq, ord, yojson, show]

val ticket_transfer :
  identity:Identity.t ->
  level:Level.t ->
  nonce:Nonce.t ->
  receiver:Address.t ->
  amount:Amount.t ->
  operation

val noop : identity:Identity.t -> level:Level.t -> nonce:Nonce.t -> operation

val is_in_includable_window :
  current_level:Level.t -> operation_level:Level.t -> bool
