open Deku_tezos

type address = private
  | Address of { contract : Contract.t; entrypoint : string }

type t = address [@@deriving show, eq, ord]

val of_contract : contract:Contract.t -> entrypoint:string -> address
val of_tezos_address : Tezos_address.t -> address

(* repr *)
val encoding : address Data_encoding.t
val of_b58 : string -> address option
val to_b58 : address -> string
