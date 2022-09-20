type network
type t = network

val listen :
  port:int ->
  on_message:(raw_expected_hash:string -> raw_content:string -> unit) ->
  unit

val connect : nodes:Uri.t list -> network

val broadcast :
  raw_expected_hash:string -> raw_content:string -> network -> unit
