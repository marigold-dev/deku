open Deku_crypto

type tezos_contract =
  | Tezos_contract_implicit of Key_hash.t
  | Tezos_contract_originated of Tezos_contract_hash.t

type t = tezos_contract [@@deriving show, eq, ord]

val of_key_hash : Key_hash.t -> tezos_contract
val of_tezos_contract_hash : Tezos_contract_hash.t -> tezos_contract

(* repr *)
val encoding : tezos_contract Data_encoding.t
val to_b58 : tezos_contract -> string
val of_b58 : string -> tezos_contract option
