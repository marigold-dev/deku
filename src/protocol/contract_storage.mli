open Contracts.Contract_vm

type storage
type t = storage

val empty : t

val originate_contract :
  address:Contract_address.t -> contract:Contract.t -> t -> t

val get_contract : t -> address:Contract_address.t -> Contract.t option

val update_contract_storage :
  address:Contract_address.t -> updated_contract:Contract.t -> t -> t
