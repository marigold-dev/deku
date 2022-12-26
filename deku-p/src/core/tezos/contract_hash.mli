open Deku_crypto
open BLAKE2b

type t = BLAKE2b_160.t [@@deriving eq, ord, show]

val encoding : t Data_encoding.t
val to_b58 : t -> string
val of_b58 : string -> t option
