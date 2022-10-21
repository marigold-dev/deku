open Deku_tezos

type storage = { set : string -> string -> unit; get : string -> string option }

val main :
  named_pipe_path:string ->
  External_vm_protocol.set list ->
  (storage:storage ->
  source:Deku_crypto.Key_hash.t ->
  tickets:(Ticket_id.t * int64) list ->
  operation:string ->
  operation_raw_hash:string ->
  (unit, string) result) ->
  unit
