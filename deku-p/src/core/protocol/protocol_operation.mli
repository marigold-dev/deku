open Deku_stdlib
open Deku_crypto
open Deku_concepts

exception Invalid_signature
exception Invalid_source

module Internal : sig
  type internal_operation = private
    | Internal_operation_register of {
        sender : Contract.t;
        ticket_id : Ticket_id.t;
      }
    | Internal_operation_transfer of {
        sender : Contract.t;
        sender_code : Ledger_code.t;
        receiver_code : Ledger_code.t;
        amount : Amount.t;
      }
    | Internal_operation_noop of { sender : Contract.t }

  type t = internal_operation [@@deriving show]
end

module Initial : sig
  module Hash : sig
    type initial_operation_hash
    and t = initial_operation_hash [@@deriving show]
  end

  module Nonce : sig
    type nonce
    type t = nonce [@@deriving show]

    (* repr *)
    val of_n : N.t -> nonce
    val to_n : nonce -> N.t
  end

  type initial_operation = private
    | Initial_operation of {
        hash : Hash.t;
        nonce : Nonce.t;
        level : Level.t;
        source : Key_hash.t;
        operation : Internal.t;
      }

  type t = initial_operation [@@deriving show]

  (* helpers *)
  val last_includable_level : operation_level:Level.t -> Level.t

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

  val transfer :
    identity:Identity.t ->
    nonce:Initial.Nonce.t ->
    level:Level.t ->
    sender_code:Ledger_code.t ->
    receiver_code:Ledger_code.t ->
    amount:Amount.t ->
    signed_operation

  val noop :
    identity:Identity.t ->
    nonce:Initial.Nonce.t ->
    level:Level.t ->
    signed_operation
end

module Raw : sig
  module Hash : sig
    type raw_operation_hash
    and t = raw_operation_hash [@@deriving show]

    module Set : Set.S with type elt = raw_operation_hash
    module Map : Map.S with type key = raw_operation_hash
  end

  type raw_operation
  type t = raw_operation [@@deriving show]

  (* constructor *)
  val of_signed : Signed.t -> raw_operation

  (* access *)
  val hash : raw_operation -> Hash.t
  val level : raw_operation -> Level.t

  (* operations *)
  val verify : raw_operation -> Signed.t option

  (* repr *)
  val encoding : raw_operation Data_encoding.t
end
