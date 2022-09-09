open Deku_indexer

type t = private {
  indexer : Indexer.t;
  consensus : Deku_tezos.Address.t;
  discovery : Deku_tezos.Address.t;
  websockets : Websocket.t Websocket.Map.t;
  random : Random.State.t;
  node : Uri.t;
}

val make :
  database_uri:Uri.t ->
  consensus:Deku_tezos.Address.t ->
  discovery:Deku_tezos.Address.t ->
  node:Uri.t ->
  unit Lwt.t

val get_state : unit -> t
val register_websocket : Websocket.t -> t -> Uuidm.t
val remove_websocket : Uuidm.t -> unit
