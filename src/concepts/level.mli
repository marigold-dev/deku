open Deku_stdlib

type level
type t = level [@@deriving eq, ord, yojson]

val zero : level
val next : level -> level
val of_n : N.t -> level
val to_n : level -> N.t
val ( < ) : level -> level -> bool

module Map : Map.S with type key = level
