open Crypto

type t [@@deriving ord, eq, yojson, bin_io]

val of_user_operation_hash : BLAKE2B.t -> t

val to_string : t -> string

val of_string : string -> t option
