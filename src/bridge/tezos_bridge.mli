open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_tezos

type bridge
type t = bridge

val make :
  rpc_node:Uri.t -> secret:Secret.t -> consensus_contract:Address.t -> t

module Consensus : sig
  val commit_state_hash :
    t ->
    block_level:Level.t ->
    block_content:BLAKE2b.t ->
    signatures:(Key.t * Signature.t) option list ->
    unit Lwt.t
  (** ~signatures should be in the same order as the old validators *)
end
