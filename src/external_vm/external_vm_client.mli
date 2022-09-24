open Deku_tezos
open Deku_crypto
open External_vm_protocol

exception Vm_lifecycle_error of string
exception Vm_execution_error of string

val get_initial_state : unit -> State.t
val set_initial_state : State.t -> unit
val start_vm_ipc : named_pipe_path:string -> unit

val apply_vm_operation_exn :
  state:State.t ->
  source:Key_hash.t ->
  tickets:(Ticket_id.t * int64) list ->
  string ->
  State.t

val close_vm_ipc : unit -> unit
