type t

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t

type frame_buffer = [ `Black | `Dark_gray | `Light_gray | `White ] array array
[@@deriving ord, eq, yojson, show]

val show : t -> string
val empty : t
val init : rom_path:string -> t
val advance : t -> frame_buffer
