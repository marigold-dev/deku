open Deku_stdlib
open Deku_chain
open Deku_network
open Deku_indexer
open Deku_tezos_interop
open Deku_consensus

type node = private {
  pool : Parallel.Pool.t;
  dump : Chain.t -> unit;
  network : Network.t;
  indexer : Indexer.t option;
  tezos_interop : Tezos_interop.t option;
  mutable chain : Chain.t;
  mutable trigger_timeout : unit -> unit;
  notify_api : Block.t -> unit;
}

type t = node

val make :
  pool:Parallel.Pool.pool ->
  dump:(Chain.t -> unit) ->
  chain:Chain.t ->
  nodes:Uri.t list ->
  ?indexer:Indexer.t option ->
  ?tezos_interop:Tezos_interop.t option ->
  notify_api:(Block.t -> unit) ->
  unit ->
  node * unit Lwt.t

val listen : node -> port:int -> tezos_interop:Tezos_interop.t -> unit
