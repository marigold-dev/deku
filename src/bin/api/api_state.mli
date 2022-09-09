open Deku_indexer
open Deku_tezos

type t = private {
  indexer : Indexer.t;
  consensus : Address.t;
  discovery : Address.t;
  websockets : Websocket.t Websocket.Map.t;
  random : Random.State.t;
}

val make :
  database_uri:Uri.t -> consensus:Address.t -> discovery:Address.t -> unit Lwt.t

val get_state : unit -> t
val register_websocket : Websocket.t -> t -> Uuidm.t
val remove_websocket : Uuidm.t -> unit
