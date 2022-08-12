open Deku_stdlib
open Deku_crypto
open Deku_tezos
open Deku_concepts

type t

val make :
  rpc_node:Uri.t ->
  secret:Secret.t ->
  consensus_contract:Address.t ->
  discovery_contract:Address.t ->
  required_confirmations:int ->
  t

module Consensus : sig
  val commit_state_hash :
    t ->
    block_level:Level.t ->
    block_payload_hash:BLAKE2b.t ->
    state_hash:BLAKE2b.t ->
    withdrawal_handles_hash:BLAKE2b.t ->
    validators:Key_hash.t list ->
    signatures:(Key.t * Signature.t) option list ->
    unit Lwt.t
  (** ~signatures should be in the same order as the old validators *)

  type transaction =
    | Deposit of {
        ticket : Tezos_ticket_id.t;
        amount : Z.t;
        destination : Key_hash.t;
      }
    | Update_root_hash of BLAKE2b.t

  type operation = {
    hash : Tezos_operation_hash.t;
    transactions : transaction list;
  }

  val listen_operations : t -> on_operation:(operation -> unit) -> unit

  val fetch_validators :
    t -> ((Key_hash.t * Uri.t option) list, string) result Lwt.t
end
