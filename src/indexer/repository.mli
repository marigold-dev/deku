open Protocol

val init : uri:Uri.t -> int64 -> unit Lwt.t
(** Loads the repository from the disk**)

val add : Block.t -> (Block.t, string) result Lwt.t
(** Adds a block in the repository **)

val level : unit -> int64 Lwt.t
(** Gets the current level of the chain *)

(* Returns a block at given level *)
val find_block_by_level : int64 -> Block.t option Lwt.t

(* Returns a block between an interval *)
val find_blocks_between : int64 -> int64 -> Block.t list Lwt.t
