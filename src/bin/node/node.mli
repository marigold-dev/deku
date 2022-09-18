open Deku_stdlib
open Deku_concepts
open Deku_indexer
open Deku_tezos_interop

type node
type t = node

val make :
  pool:Parallel.Pool.pool ->
  identity:Identity.identity ->
  nodes:(Deku_crypto.Key_hash.t * Uri.t) list ->
  ?indexer:Indexer.t option ->
  default_block_size:int ->
  unit ->
  node * unit Lwt.t

val listen : node -> port:int -> tezos_interop:Tezos_interop.t -> unit
