include module type of struct
  include Z
end

exception Not_a_string
exception Not_an_integer

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t