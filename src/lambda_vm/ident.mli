type t [@@deriving yojson, eq, ord, bin_io]

val initial : t

val next : t -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
