val make_pipe_pair : string -> unit
(** [make_pipe_pair path] idempotently creates two named pipes.
    The first is named <path>_read and the second is named <path>_write*)

val get_pipe_pair_file_descriptors :
  is_chain:bool -> string -> Unix.file_descr * Unix.file_descr
(** [get_pipe_pair_file_descriptors path] returns a pair or file descriptors
    created with [make_pipe_pair] opened for writing and writing respectively. *)
