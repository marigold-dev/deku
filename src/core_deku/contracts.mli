module Conversions :
  Smart_contracts.Conversions
    with type Address.t = Address.t
     and type Amount.t = Amount.t
     and type Ticket_id.t = Ticket_id.t

module Context :
  Smart_contracts.CTX
    with type Address.t = Address.t
     and type Amount.t = Amount.t
     and type Ticket_id.t = Ticket_id.t

module Contract_vm : module type of Smart_contracts.Contract_vm.Make (Context)
