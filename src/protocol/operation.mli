open Deku_crypto
open Deku_concepts

exception Invalid_signature

type operation_data = private
  | Operation_transaction of { receiver : Address.t; amount : Amount.t }

type operation = private
  | Operation of {
      (* FIXME: why don't we use verified_signature here? *)
      key : Key.t;
      signature : Signature.t;
      hash : Operation_hash.t;
      level : Level.t;
      nonce : Nonce.t;
      source : Address.t;
      data : operation_data;
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

module Set : Set.S with type elt = operation
