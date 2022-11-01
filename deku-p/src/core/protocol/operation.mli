open Deku_crypto
open Deku_concepts
open Deku_ledger

exception Invalid_signature
exception Invalid_source

type operation = private
  | Operation_ticket_transfer of {
      sender : Address.t;
      receiver : Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_vm_transaction of {
      sender : Address.t;
      operation : Ocaml_wasm_vm.Operation.t;
    }
  | Operation_withdraw of {
      sender : Address.t;
      owner : Deku_tezos.Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_noop of { sender : Address.t }

type t = operation [@@deriving show, yojson]

val encoding : t Data_encoding.t

module Initial : sig
  type initial_operation = private
    | Initial_operation of {
        hash : Operation_hash.t;
        nonce : Nonce.t;
        level : Level.t;
        operation : operation;
      }

  type t = initial_operation [@@deriving show, yojson]

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

  type t = signed_operation [@@deriving show, yojson]

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

  val vm_transaction :
    nonce:Nonce.t ->
    level:Level.t ->
    content:Ocaml_wasm_vm.Operation.t ->
    identity:Identity.t ->
    signed_operation

  val noop :
    identity:Identity.t -> nonce:Nonce.t -> level:Level.t -> signed_operation
end
