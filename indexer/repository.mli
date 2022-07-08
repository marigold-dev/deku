open Protocol

val init : unit -> unit Lwt.t
(** Loads the repository from the disk**)

val add : Block.t -> Block.t Lwt.t
(** Adds a block in the repository **)

val level : unit -> int64
(** Gets the current level of the chain *)
