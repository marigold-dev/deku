open Deku_crypto
open Deku_stdlib
open External_vm_protocol

module Make (Ticket_id : sig
  type t [@@deriving yojson]
end) (Address : sig
  type t [@@deriving yojson]
end) : sig
  exception Vm_lifecycle_error of string
  exception Vm_execution_error of string

  val get_initial_state : unit -> State.t
  val set_initial_state : State.t -> unit
  val start_vm_ipc : named_pipe_path:string -> unit

  type ledger_api =
    < take_tickets : Address.t -> (Ticket_id.t * N.t) list
    ; deposit : Address.t -> Ticket_id.t * N.t -> unit >

  val apply_vm_operation_exn :
    state:State.t ->
    ledger_api:< ledger_api ; .. > ->
    source:Key_hash.t ->
    tickets:(Ticket_id.t * N.t) list ->
    level:Deku_concepts.Level.t ->
    (BLAKE2b.t * string) option ->
    State.t

  val close_vm_ipc : unit -> unit
end
