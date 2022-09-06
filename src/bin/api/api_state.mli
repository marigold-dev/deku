open Deku_indexer

type t = private { indexer : Indexer.t }

val make : database_uri:Uri.t -> unit Lwt.t
val get_state : unit -> t
