type t [@@deriving yojson]

val compare : t -> t -> int
val equal : t -> t -> bool

val initial : t
val next : t -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
