(* TODO: better API *)
val spawn :
  file:string ->
  on_error:(exn -> unit) ->
  on_json:(Yojson.Safe.t -> unit) ->
  (write:(Yojson.Safe.t -> unit) -> unit) ->
  'a
