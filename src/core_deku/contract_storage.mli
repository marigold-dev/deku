open Deku_data
open Contracts.Contract_vm

type t [@@deriving yojson, eq]

val empty : t

val originate_contract :
  t -> address:Contract_address.t -> contract:Contract.t -> t

val update_contract_storage :
  t -> address:Contract_address.t -> updated_contract:Contract.t -> t

val get_contract : t -> address:Contract_address.t -> Contract.t option
