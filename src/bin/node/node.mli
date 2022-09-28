open Deku_stdlib
open Deku_chain

type node
type t = node

val make :
  pool:Parallel.Pool.t -> dump:(Chain.t -> unit) -> chain:Chain.t -> node

val start :
  sw:Eio.Switch.t ->
  env:Eio.Stdenv.t ->
  port:int ->
  nodes:(string * int) list ->
  node ->
  unit

val test : unit -> unit
