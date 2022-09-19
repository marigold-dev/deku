open Deku_stdlib
open Deku_concepts
open Deku_indexer
open Deku_tezos_interop
open Deku_crypto

type node
type t = node

val make :
  pool:Parallel.Pool.pool ->
  identity:Identity.identity ->
  validators:Key_hash.t list ->
  nodes:Uri.t list ->
  ?indexer:Indexer.t option ->
  default_block_size:int ->
  unit ->
  node * unit Lwt.t

val listen : node -> port:int -> tezos_interop:Tezos_interop.t -> unit
