open Deku_stdlib
open Deku_crypto

type address
type t = address [@@deriving eq, ord, yojson, show]

(* repr *)
val of_key_hash : Key_hash.t -> address
val to_key_hash : address -> Key_hash.t option
val of_contract_address : Contract_address.t -> t
val to_contract_address : t -> Contract_address.t option
val of_b58 : string -> address option
val to_b58 : address -> string

(* TODO: where this is used? *)
module Map : Map.S with type key = address

val cmdliner_converter :
  (string -> [> `Ok of t | `Error of string ]) * (Format.formatter -> t -> unit)
