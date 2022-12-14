open Deku_crypto

type deku_contract_address
type t = deku_contract_address [@@deriving ord, eq, yojson, show]

val of_user_operation_hash : BLAKE2b.t -> t
val to_b58 : deku_contract_address -> string
val of_b58 : string -> deku_contract_address option
val encoding : deku_contract_address Data_encoding.t

module Map : Map.S with type key = deku_contract_address
