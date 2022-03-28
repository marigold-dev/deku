open Contract_vm

type t [@@deriving yojson]

val empty : t
val equal : t -> t -> bool
val originate_contract :
  t -> address:Contract_address.t -> contract:Contract.t -> t
val update_contract_storage :
  t -> address:Contract_address.t -> updated_contract:Contract.t -> t
val get_contract : t -> address:Contract_address.t -> Contract.t option
