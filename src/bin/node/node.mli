open Deku_stdlib
open Deku_chain
open Deku_indexer
open Deku_tezos_interop

type node
type t = node

val make :
  pool:Parallel.Pool.pool ->
  dump:(Chain.t -> unit) ->
  chain:Chain.t ->
  nodes:Uri.t list ->
  ?indexer:Indexer.t option ->
  unit ->
  node * unit Lwt.t

val listen : node -> port:int -> tezos_interop:Tezos_interop.t -> unit
