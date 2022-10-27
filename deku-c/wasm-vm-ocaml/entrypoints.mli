type direction = Left | Right [@@deriving eq, ord, show]

val direction_encoding : direction Data_encoding.t

type t [@@deriving show, eq, ord]

val get_path : t -> string -> direction list option
val encoding : t Data_encoding.t
val of_assoc : (string * direction list) list -> t
