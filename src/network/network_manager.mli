open Deku_gossip

type network
type t = network

val make : unit -> network

val listen :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  port:int ->
  on_request:
    (connection:Connection_id.t ->
    raw_expected_hash:string ->
    raw_content:string ->
    unit) ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  network ->
  'a

val connect :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  nodes:(string * int) list ->
  on_request:
    (connection:Connection_id.t ->
    raw_expected_hash:string ->
    raw_content:string ->
    unit) ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  network ->
  unit

val broadcast :
  raw_expected_hash:string -> raw_content:string -> network -> unit

val request : raw_expected_hash:string -> raw_content:string -> network -> unit

val send :
  connection:Connection_id.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  unit

val test : unit -> unit
