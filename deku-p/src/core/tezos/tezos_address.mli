type tezos_address = private
  | Tezos_address of { contract : Tezos_contract.t; entrypoint : string }

type t = tezos_address [@@deriving show, eq, ord]

val make : contract:Tezos_contract.t -> entrypoint:string -> tezos_address

(* repr *)
val encoding : tezos_address Data_encoding.t
val to_b58 : tezos_address -> string
val of_b58 : string -> tezos_address option

val cmdliner_converter :
  (string -> [> `Ok of tezos_address | `Error of string ])
  * (Format.formatter -> tezos_address -> unit)
