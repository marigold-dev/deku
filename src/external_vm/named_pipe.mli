val make_pipe_pair : string -> unit
(** [make_pipe_pair path] idempotently creates two named pipes.
    The first is named <path>_read and the second is named <path>_write*)

val get_pipe_pair_file_descriptors : string -> Unix.file_descr * Unix.file_descr
(** [get_pipe_pair_file_descriptors path] returns a pair or file descriptors
    created with [make_pipe_pair] opened for writing and writing respectively. *)

val get_pipe_pair_channels :
  string -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
