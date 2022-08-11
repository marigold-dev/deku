open Deku_crypto
open Deku_concepts

exception Invalid_signature

type operation_content = private
  | Operation_transaction of { receiver : Address.t; amount : Amount.t }
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

type t = operation [@@deriving eq, ord, yojson]

val transaction :
  identity:Identity.t ->
  level:Level.t ->
  nonce:Nonce.t ->
  source:Address.t ->
  receiver:Address.t ->
  amount:Amount.t ->
  operation

val noop : level:Level.t -> operation
