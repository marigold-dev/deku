open Deku_stdlib

type level
type t = level [@@deriving show, eq, ord]

val zero : level
val next : level -> level
val of_n : N.t -> level
val to_n : level -> N.t
val ( < ) : level -> level -> bool
