val broadcast_json :
  nodes:Uri.t list ->
  endpoint:_ Endpoint.endpoint ->
  message:Yojson.Safe.t ->
  unit

val broadcast_message :
  nodes:Uri.t list -> endpoint:_ Endpoint.endpoint -> message:Message.t -> unit
