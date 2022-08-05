type contract_hash
type t = contract_hash [@@deriving eq, ord, yojson]

(* repr *)
val encoding : contract_hash Data_encoding.t
val of_b58 : string -> contract_hash option
val to_b58 : contract_hash -> string

(* operations *)
val hash : string -> contract_hash

module Map : Map.S with type key = contract_hash
