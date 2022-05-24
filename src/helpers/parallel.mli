val encode : ('a -> Yojson.Safe.t) -> 'a -> string Lwt.t

val decode : (Yojson.Safe.t -> 'a) -> string -> 'a Lwt.t
