val listen :
  sw:Eio.Switch.t ->
  net:#Eio.Net.t ->
  port:int ->
  on_error:(exn -> unit) ->
  on_request:
    (send:(raw_expected_hash:string -> raw_content:string -> unit) ->
    raw_expected_hash:string ->
    raw_content:string ->
    unit) ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  unit

exception Invalid_host

val connect :
  sw:Eio.Switch.t ->
  net:#Eio.Net.t ->
  host:string ->
  port:int ->
  on_request:
    (send:(raw_expected_hash:string -> raw_content:string -> unit) ->
    raw_expected_hash:string ->
    raw_content:string ->
    unit) ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  (request:(raw_expected_hash:string -> raw_content:string -> unit) ->
  send:(raw_expected_hash:string -> raw_content:string -> unit) ->
  unit) ->
  unit

val test : unit -> unit
