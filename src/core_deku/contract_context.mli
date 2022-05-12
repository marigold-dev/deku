module CTX :
  Contracts.CTX
    with type Conv.Address.t = Address.t
     and type Conv.Amount.t = Amount.t
     and type Conv.Ticket_handle.t = Ticket_handle.t
     and type Conv.Ticket_id.t = Ticket_id.t
     and type Conv.Table.t = Ticket_transition_table.t
     and type Conv.Operation.t = Contract_operation.t

val make_state :
  get_contract_opt:(Address.t -> Address.t option) ->
  source:Address.t ->
  sender:Address.t ->
  self:Address.t ->
  contract_owned_tickets:(Ticket_id.t * Amount.t) Seq.t ->
  provided_tickets:(Ticket_id.t * Amount.t) Seq.t ->
  CTX.State.full_state
