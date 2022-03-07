type t [@@deriving eq]

val compare : t -> t -> int
val initial : t
val next : t -> t
