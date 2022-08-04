open Deku_stdlib
open Deku_concepts
open Deku_crypto

type node
type t = node

val make :
  pool:Parallel.Pool.pool ->
  identity:Identity.identity ->
  validators:Deku_crypto.Key_hash.t list ->
  nodes:Uri.t list ->
  bootstrap_key:Key.t ->
  node * unit Lwt.t

val listen : node -> port:int -> unit
