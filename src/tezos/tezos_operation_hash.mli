type t [@@deriving eq, ord, yojson]

val of_b58 : string -> t option
val to_b58 : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
