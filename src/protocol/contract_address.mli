open Deku_crypto

type t = BLAKE2b.BLAKE2b_160.t [@@deriving eq, ord, yojson]

val of_user_operation_hash :
  Operation_hash.operation_hash -> Deku_crypto.BLAKE2b.BLAKE2b_160.hash

(*repr*)
val of_b58 : string -> t option
val to_b58 : t -> string
