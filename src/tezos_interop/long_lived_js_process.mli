(** Interface between a long lived JS script and an OCaml consumer *)

type t
val spawn : file:string -> t
val listen :
  t ->
  to_yojson:('request -> Yojson.Safe.t) ->
  of_yojson:(Yojson.Safe.t -> ('message, string) result) ->
  'request ->
  'message Lwt_stream.t
val request :
  t ->
  to_yojson:('request -> Yojson.Safe.t) ->
  of_yojson:(Yojson.Safe.t -> ('response, string) result) ->
  'request ->
  'response Lwt.t
