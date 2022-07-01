open Crypto

type t [@@deriving eq, ord, yojson]

val is_implicit : t -> bool

val is_originated : t -> bool

val of_key_hash : Key_hash.t -> t

val to_key_hash : t -> Key_hash.t option

val of_contract_hash : Contract_address.t -> t

val to_contract_hash : t -> Contract_address.t option

val to_string : t -> string

val of_string : string -> t option
