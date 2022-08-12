open Deku_crypto

type address
type t = address [@@deriving eq, ord, yojson]

val is_implicit : address -> bool
val is_originated : address -> bool

(* repr *)
val of_key_hash : Key_hash.t -> address
val of_contract_hash : Contract_address.t -> address
val to_key_hash : address -> Key_hash.t option
val to_contract_hash : address -> Contract_address.t option
val of_b58 : string -> address option
val to_b58 : address -> string

(* TODO: where this is used? *)
module Map : Map.S with type key = address
