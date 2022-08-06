type t [@@deriving eq, ord]

val of_b58 : string -> t option
val to_b58 : t -> string
