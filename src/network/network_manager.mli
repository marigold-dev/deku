open Deku_gossip

type network
type t = network

val make : unit -> network

val listen :
  sw:Eio.Switch.t ->
  env:Eio.Stdenv.t ->
  port:int ->
  on_request:
    (id:Request_id.t -> raw_expected_hash:string -> raw_content:string -> unit) ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  network ->
  unit

val connect :
  sw:Eio.Switch.t ->
  env:Eio.Stdenv.t ->
  nodes:(string * int) list ->
  on_request:
    (id:Request_id.t -> raw_expected_hash:string -> raw_content:string -> unit) ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  network ->
  unit

val broadcast :
  sw:Eio.Switch.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  unit

val request :
  sw:Eio.Switch.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  unit

val respond :
  id:Request_id.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  unit

val not_found : id:Request_id.t -> network -> unit
val test : unit -> unit
