type t = { ticketer : Address.t; data : bytes } [@@deriving eq, ord, yojson]

val to_string : t -> string
val of_string : string -> t option
val encoding : t Data_encoding.t
