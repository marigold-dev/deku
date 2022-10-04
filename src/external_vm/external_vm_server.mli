open Deku_stdlib

module Make (Ticket_id : sig
  type t [@@deriving yojson]
end) (Address : sig
  type t [@@deriving yojson]
end) : sig
  type storage = {
    set : string -> string -> unit;
    get : string -> string option;
  }

  val main :
    named_pipe_path:string ->
    External_vm_protocol.set list ->
    (storage:storage ->
    source:Deku_crypto.Key_hash.t ->
    tickets:(Ticket_id.t * N.t) list ->
    operation:string ->
    operation_raw_hash:string ->
    (unit, string) result) ->
    unit
end
