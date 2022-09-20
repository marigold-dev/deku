open Deku_stdlib
open Deku_chain

type node
type t = node

val make :
  pool:Parallel.Pool.pool ->
  dump:(Chain.t -> unit) ->
  chain:Chain.t ->
  nodes:Uri.t list ->
  node * unit Lwt.t

val listen : node -> port:int -> unit
val test : unit -> unit
