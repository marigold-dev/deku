open Deku_stdlib
open Deku_concepts
open Deku_chain
open Deku_network
open Deku_indexer
open Deku_tezos_interop
open Deku_consensus

type node = private {
  identity : Identity.t;
  default_block_size : int;
  dump : Chain.t -> unit;
  network : Network_manager.t;
  indexer : Indexer.t option;
  mutable tezos_interop : Tezos_interop.t option;
  mutable chain : Chain.t;
  mutable cancel : unit -> unit;
  notify_api : Block.t -> unit;
  preferred_fee : int option;
}

type t = node

val make :
  identity:Identity.t ->
  default_block_size:int ->
  dump:(Chain.t -> unit) ->
  chain:Chain.t ->
  indexer:Indexer.t option ->
  notify_api:(Block.t -> unit) ->
  preferred_fee:int option ->
  node

val start :
  sw:Eio.Switch.t ->
  env:Eio.Stdenv.t ->
  port:int ->
  nodes:(string * int) list ->
  tezos:(Uri.t * Deku_crypto.Secret.secret * Deku_tezos.Address.t) option ->
  node ->
  unit

val test : unit -> unit
