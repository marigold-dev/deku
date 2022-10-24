type storage = { set : string -> string -> unit; get : string -> string option }

open Deku_stdlib

val main :
  named_pipe_path:string ->
  External_vm_protocol.set list ->
  (storage:storage ->
  source:Deku_crypto.Key_hash.t ->
  tickets:(Deku_ledger.Ticket_id.t * N.t) list ->
  operation:string ->
  operation_raw_hash:string ->
  (unit, string) result) ->
  unit
