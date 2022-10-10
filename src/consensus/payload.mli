type payload = Payload of string list
type t = payload

val encode : payload:payload -> string
val decode : payload:string -> payload
