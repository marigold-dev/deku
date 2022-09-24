open Deku_tezos

type storage = { set : string -> string -> unit; get : string -> string option }

val main :
  named_pipe_path:string ->
  External_vm_protocol.set list ->
  (storage ->
  Deku_crypto.Key_hash.t ->
  (Ticket_id.t * int64) list ->
  string ->
  (unit, string) result) ->
  unit
