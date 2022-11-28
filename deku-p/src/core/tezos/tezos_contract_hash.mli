type tezos_contract_hash
type t = tezos_contract_hash [@@deriving show, eq, ord]

val encoding : tezos_contract_hash Data_encoding.t
val to_b58 : tezos_contract_hash -> string
val of_b58 : string -> tezos_contract_hash option
