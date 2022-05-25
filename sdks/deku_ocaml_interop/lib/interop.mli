val main : (string * Yojson.Safe.t) list -> (string -> string -> bytes -> (unit, string) result) -> unit
val get : string -> Yojson.Safe.t
val set : string -> Yojson.Safe.t -> unit