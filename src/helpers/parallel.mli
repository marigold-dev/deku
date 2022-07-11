val encode : ('a -> Yojson.Safe.t) -> 'a -> string Lwt.t

val decode : (Yojson.Safe.t -> 'a) -> string -> 'a Lwt.t

val protocol_decode_list :
  (Yojson.Safe.t -> ('a, 'err) result) ->
  string list ->
  ('a, [> `Of_exception of exn | `Of_yojson    of 'err]) result list
