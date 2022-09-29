open Deku_stdlib
open Deku_crypto
open Deku_tezos
open Deku_concepts

type interop
type t = interop

type transaction =
  private
  | Deposit of { ticket : Ticket_id.t; amount : Z.t; destination : Key_hash.t }
  | Update_root_hash of BLAKE2b.t

type operation = private {
  hash : Tezos_operation_hash.t;
  transactions : transaction list;
}

val start :
  sw:Eio.Switch.t ->
  rpc_node:Uri.t ->
  secret:Secret.t ->
  consensus_contract:Address.t ->
  on_operation:(operation -> unit) ->
  t

val commit_state_hash :
  interop ->
  block_level:Level.t ->
  block_payload_hash:BLAKE2b.t ->
  state_hash:BLAKE2b.t ->
  withdrawal_handles_hash:BLAKE2b.t ->
  validators:Key_hash.t list ->
  (** ~signatures should be in the same order as the old validators *)
  signatures:(Key.t * Signature.t) option list ->
  unit

