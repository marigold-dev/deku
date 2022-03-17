type t [@@deriving eq, show]

val compare : t -> t -> int
val initial : t
val next : t -> t
