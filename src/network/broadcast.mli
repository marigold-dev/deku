val broadcast_json :
  nodes:Uri.t list ->
  endpoint:_ Endpoint.endpoint ->
  packet:Yojson.Safe.t ->
  unit

val broadcast_packet :
  nodes:Uri.t list -> endpoint:_ Endpoint.endpoint -> packet:Packet.t -> unit
