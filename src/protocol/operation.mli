open Deku_crypto
open Deku_concepts

type operation_data = private
  | Operation_transaction of { receiver : Address.t; amount : Amount.t }

type operation = private
  | Operation of {
      hash : Operation_hash.t;
      level : Level.t;
      nonce : Nonce.t;
      source : Address.t;
      data : operation_data;
    }

type t = operation [@@deriving eq, ord, yojson]

val transaction :
  level:Level.t ->
  nonce:Nonce.t ->
  source:Address.t ->
  receiver:Address.t ->
  amount:Amount.t ->
  operation

val verify : Key.t -> Signature.t -> operation -> bool

module Set : Set.S with type elt = operation
