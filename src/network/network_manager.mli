open Deku_gossip

type network
type t = network

val make : unit -> network

val listen :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  port:int ->
  on_connection:(connection:Connection_id.t -> unit) ->
  on_request:
    (connection:Connection_id.t ->
    raw_header:string ->
    raw_fragments:string list ->
    unit) ->
  on_message:(raw_header:string -> raw_fragments:string list -> unit) ->
  network ->
  'a

val connect :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  nodes:(string * int) list ->
  on_connection:(connection:Connection_id.t -> unit) ->
  on_request:
    (connection:Connection_id.t ->
    raw_header:string ->
    raw_fragments:string list ->
    unit) ->
  on_message:(raw_header:string -> raw_fragments:string list -> unit) ->
  network ->
  unit

val broadcast :
  raw_header:string -> raw_fragments:string list -> network -> unit

val request : raw_header:string -> raw_fragments:string list -> network -> unit

val send :
  connection:Connection_id.t ->
  raw_header:string ->
  raw_fragments:string list ->
  network ->
  unit

val send_request :
  connection:Connection_id.t ->
  raw_header:string ->
  raw_fragments:string list ->
  network ->
  unit

val test : unit -> unit
