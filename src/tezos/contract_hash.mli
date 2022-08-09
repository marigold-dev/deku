open Deku_crypto
open BLAKE2b

type t = BLAKE2b_160.t [@@deriving eq, ord, yojson]

val encoding : t Data_encoding.t
val to_string : t -> string
val of_string : string -> t option
