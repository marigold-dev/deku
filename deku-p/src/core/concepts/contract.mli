open Deku_crypto
open Deku_tezos

type contract = private
  | Contract_implicit of Key_hash.t
  | Contract_tezos_originated of Tezos_contract_hash.t
  | Contract_deku_originated of Deku_contract_address.t

type t = contract [@@deriving show, eq, ord]

(* tz1, tz2, tz3 *)
val of_key_hash : Key_hash.t -> contract

(* KT1 *)
val of_tezos_contract_hash : Tezos_contract_hash.t -> contract

(* DK1 *)
val of_deku_contract_address : Deku_contract_address.t -> contract

(* tz1, tz2, tz3, KT1 *)
val of_tezos_contract : Tezos_contract.t -> contract

(* repr *)
val encoding : contract Data_encoding.t
val of_b58 : string -> contract option
val to_b58 : contract -> string
