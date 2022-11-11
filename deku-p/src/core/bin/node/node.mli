open Deku_stdlib
open Deku_concepts
open Deku_chain
open Deku_network
open Deku_block_storage
open Deku_tezos_interop

type node = private {
  identity : Identity.t;
  default_block_size : int;
  dump : Chain.t -> unit;
  network : Network_manager.t;
  indexer : Block_storage.t option;
  mutable tezos_interop : Tezos_interop.t option;
  mutable chain : Chain.t;
  mutable cancel : unit -> unit;
}

type t = node

val make :
  identity:Identity.t ->
  default_block_size:int ->
  dump:(Chain.t -> unit) ->
  chain:Chain.t ->
  indexer:Block_storage.t option ->
  node

val start :
  sw:Eio.Switch.t ->
  env:Eio.Stdenv.t ->
  port:int ->
  nodes:(string * int) list ->
  tezos:(Uri.t * Deku_crypto.Secret.secret * Deku_tezos.Address.t) option ->
  node ->
  unit
