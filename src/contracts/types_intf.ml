module S = struct
  type address

  type ticket_handle

  type amount

  type ticket_repr

  type table

  type operation
end

module type S = module type of S

module Make (CC : Conv.S) :
  S
    with type address = CC.Address.t
     and type table = CC.Table.t
     and type ticket_repr = CC.Ticket_id.t
     and type ticket_handle = CC.Ticket_handle.t
     and type operation = CC.Operation.t
     and type amount = CC.Amount.t = struct
  type address = CC.Address.t

  type ticket_handle = CC.Ticket_handle.t

  type amount = CC.Amount.t

  type ticket_repr = CC.Ticket_id.t

  type table = CC.Table.t

  type operation = CC.Operation.t
end
