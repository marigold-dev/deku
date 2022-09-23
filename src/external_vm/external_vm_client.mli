open External_vm_protocol

type error = [ `Vm_lifecycle_error of string | `Vm_execution_error of string ]

val get_initial_state : unit -> State.t
val set_initial_state : State.t -> unit
val start_vm_ipc : named_pipe_path:string -> unit

(* This [t option] seems like the result of a bad abstraction
     TODO: find a better way to structure this. *)
val apply_vm_operation :
  state:State.t ->
  source:Deku_crypto.Key_hash.key_hash ->
  tickets:Deku_concepts.Ticket.t list ->
  string ->
  (State.t, error) result

val close_vm_ipc : unit -> unit
