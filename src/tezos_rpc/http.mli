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
