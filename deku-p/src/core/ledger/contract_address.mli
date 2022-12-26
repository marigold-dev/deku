open Deku_crypto
open BLAKE2b

type t [@@deriving ord, eq, show]

val of_user_operation_hash : BLAKE2b_256.t -> t
val to_b58 : t -> string
val of_b58 : string -> t option
val encoding : t Data_encoding.t
