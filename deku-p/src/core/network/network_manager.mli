open Deku_concepts
open Deku_gossip

type network
type t = network

val make : identity:Identity.t -> network

val listen :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  port:int ->
  on_connection:(connection:Connection_id.t -> unit) ->
  on_message:(connection:Connection_id.t -> message:Network_message.t -> unit) ->
  network ->
  'a

val connect :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  nodes:(string * int) list ->
  on_connection:(connection:Connection_id.t -> unit) ->
  on_message:(connection:Connection_id.t -> message:Network_message.t -> unit) ->
  network ->
  unit

val send :
  connection:Connection_id.t -> message:Network_message.t -> network -> unit

val broadcast : message:Network_message.t -> network -> unit
val test : unit -> unit
