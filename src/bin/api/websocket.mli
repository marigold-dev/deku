(* Should this module be renamed to Client ? (so that we abstract the concept of websocket ?)*)
open Deku_consensus

type t = Dream.websocket
type message = New_block of Block.t

module Map : Map.S with type key = Uuidm.t

val send : t -> message -> unit Lwt.t
(** Sends a message to a client *)

val broadcast : t Map.t -> message -> unit Lwt.t
(** Broadcast a message to everyone *)
