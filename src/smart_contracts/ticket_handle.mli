type t = Int32.t [@@deriving ord, eq, yojson]

val to_string : t -> string

val of_string : string -> t

val size : int

val to_bytes : t -> bytes

val of_bytes : bytes -> t
