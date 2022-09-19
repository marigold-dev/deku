open Deku_stdlib
open Deku_crypto
open Deku_concepts

type node
type t = node

val make :
  pool:Parallel.Pool.pool ->
  identity:Identity.identity ->
  validators:Key_hash.t list ->
  nodes:Uri.t list ->
  node * unit Lwt.t

val listen : node -> port:int -> unit
val test : unit -> unit
