open Deku_stdlib
open Deku_chain
open Deku_network
open Deku_gossip
open Deku_indexer
open Deku_tezos_interop
open Deku_external_vm
open Deku_concepts
open Deku_crypto

type node = private {
  pool : Parallel.Pool.t;
  network : Network.t;
  indexer : Indexer.t option;
  mutable gossip : Gossip.t;
  mutable chain : Chain.t;
  mutable trigger_timeout : unit -> unit;
}

type t = node

val make :
  pool:Parallel.Pool.pool ->
  identity:Identity.identity ->
  validators:Deku_crypto.Key_hash.t list ->
  nodes:Uri.t list ->
  bootstrap_key:Key.t ->
  indexer:Indexer.t option ->
  default_block_size:int ->
  vm_state:External_vm_protocol.State.t ->
  node * unit Lwt.t

val listen : node -> port:int -> tezos_interop:Tezos_interop.t -> unit
