open Crypto
type t [@@deriving ord, eq, yojson]

val of_user_operation_hash : BLAKE2B.t -> t

val to_string : t -> string
val of_string : string -> t option
