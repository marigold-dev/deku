val spawn :
  file:string ->
  on_error:(exn -> unit) ->
  on_close:(Unix.process_status -> unit) ->
  Yojson.Safe.t Lwt_stream.t ->
  Yojson.Safe.t Lwt_stream.t
