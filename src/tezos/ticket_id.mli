type t = { ticketer : Address.t; data : bytes } [@@deriving eq, ord]

val to_string : t -> string
val of_string : string -> t option
