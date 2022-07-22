open Protocol

val init : int64 -> unit Lwt.t
(** Loads the repository from the disk**)

val add : Block.t -> Block.t Lwt.t
(** Adds a block in the repository **)

val level : unit -> int64
(** Gets the current level of the chain *)

(* Returns a block at given level *)
val find_block_by_level : int64 -> Block.t option Lwt.t
