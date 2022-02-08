open Crypto
type t[@@deriving (eq, ord, yojson)]
val of_key_hash : Key_hash.t -> t
val to_key_hash : t -> Key_hash.t option
val of_contract_hash : Tezos.Contract_hash.t -> t
val to_contract_hash : t -> Tezos.Contract_hash.t option
val to_string : t -> string
val of_string : string -> t option