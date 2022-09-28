(** Interface between a long lived JS script and an OCaml consumer *)

type t

val spawn : sw:Eio.Switch.t -> file:string -> t

val listen :
  t ->
  to_yojson:('request -> Yojson.Safe.t) ->
  of_yojson:(Yojson.Safe.t -> 'message) ->
  'request ->
  ('message -> unit) ->
  unit

val request :
  t ->
  to_yojson:('request -> Yojson.Safe.t) ->
  of_yojson:(Yojson.Safe.t -> 'response) ->
  'request ->
  'response
