type t [@@deriving eq, ord, yojson]

val encoding : t Data_encoding.t

val zero : t

val one_mutez : t

val one : t

val of_mutez : int64 -> t option
