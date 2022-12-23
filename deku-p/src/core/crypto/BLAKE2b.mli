module BLAKE2b_160 : Hash_intf.S
module BLAKE2b_256 : Hash_intf.S
include Hash_intf.S with type hash = BLAKE2b_256.hash

val encoding : t Data_encoding.t
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t