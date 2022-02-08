open Crypto
type t = Key.t[@@deriving (yojson, ord)]
val of_key : Secret.t -> t
val genesis_key : Secret.t
val genesis_wallet : t
val to_string : t -> string
val of_string : string -> t option