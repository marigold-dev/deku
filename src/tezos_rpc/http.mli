val http_get :
  node_uri:Uri.t ->
  path:string ->
  of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
  ('a, Error.error) result Lwt.t

val http_post :
  node_uri:Uri.t ->
  path:string ->
  of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
  data:Yojson.Safe.t ->
  ('a, Error.error) result Lwt.t

(** Makes a forever-lived HTTP request and return a stream with each
  entry on the response body. Used with Tezos /monitor endpoints. *)
val http_get_listen :
  node_uri:Uri.t ->
  path:string ->
  of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
  ('a Lwt_stream.t, Error.error) result Lwt.t

val http_post_data_encoding :
  node_uri:Uri.t ->
  path:string ->
  of_yojson:(Yojson.Safe.t -> ('a, string) result) ->
  data:Data_encoding.json ->
  ('a, Error.error) result Lwt.t
