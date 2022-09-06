open Deku_indexer
open Deku_tezos

type t = private {
  indexer : Indexer.t;
  consensus : Address.t;
  discovery : Address.t;
}

val make :
  database_uri:Uri.t -> consensus:Address.t -> discovery:Address.t -> unit Lwt.t

val get_state : unit -> t
