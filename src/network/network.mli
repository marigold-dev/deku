open Deku_gossip

type network
type t = network

val listen :
  port:int ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  (* TODO: name (string * string )*)
  on_request:
    (id:Request_id.t -> raw_expected_hash:string -> raw_content:string -> unit) ->
  network ->
  unit

val connect : nodes:Uri.t list -> network

val broadcast :
  raw_expected_hash:string -> raw_content:string -> network -> unit

(* TODO: name (string * string )*)
val request :
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  (string * string) Lwt.t

val respond :
  id:Request_id.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  unit

val not_found : id:Request_id.t -> network -> unit
