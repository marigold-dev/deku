include module type of struct
  include Z
end

(* yojson exceptions *)
exception Not_an_integer
exception Not_a_string

val z_of_yojson : Yojson.Safe.t -> Z.t
val yojson_of_z : Z.t -> Yojson.Safe.t
