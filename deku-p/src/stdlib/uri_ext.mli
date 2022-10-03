include module type of struct
  include Uri
end

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t

val cmdliner_converter :
  (string -> [> `Ok of t | `Error of string ]) * (Format.formatter -> t -> unit)
