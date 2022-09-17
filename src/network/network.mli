open Deku_crypto

type network
type t = network

val listen :
  port:int ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  unit

val connect : nodes:(Key_hash.t * Uri.t) list -> network

val send :
  to_:Key_hash.t ->
  raw_expected_hash:string ->
  raw_content:string ->
  network ->
  unit

val broadcast :
  raw_expected_hash:string -> raw_content:string -> network -> unit
