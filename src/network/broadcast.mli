val broadcast_json :
  nodes:Uri.t list ->
  endpoint:_ Endpoint.endpoint ->
  packet:Yojson.Safe.t ->
  unit

val broadcast_packet :
  nodes:Uri.t list -> endpoint:_ Endpoint.endpoint -> packet:Packet.t -> unit

(* TODO: define this function in other module ?*)
val notify_api_new_block :
  api:Uri.t -> level:Deku_concepts.Level.t -> unit Lwt.t
