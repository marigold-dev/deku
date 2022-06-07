open Crypto
open External_vm_protocol

val get_initial_state : unit -> State.t

val set_initial_state : State.t -> unit

val start_vm_ipc : named_pipe_path:string -> unit

val apply_vm_operation :
  state:
    (* This [t option] seems like the result of a bad abstraction
       TODO: find a better way to structure this. *)
    State.t option ->
  source:Key_hash.t ->
  tx_hash:BLAKE2B.t ->
  Yojson.Safe.t ->
  State.t

val close_vm_ipc : unit -> unit
