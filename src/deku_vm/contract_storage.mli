open Contract_vm
open External_vm

val initial_state : External_vm_protocol.set list

val originate_contract :
  External_vm.External_vm_server.storage ->
  address:Contract_address.t ->
  contract:Contract.t ->
  unit

val update_contract_storage :
  External_vm.External_vm_server.storage ->
  address:Contract_address.t ->
  updated_contract:Contract.t ->
  unit

val get_contract :
  External_vm.External_vm_server.storage ->
  address:Contract_address.t ->
  Contract.t option
