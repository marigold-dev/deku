module Make (CC : Conversions.S) :
  Context.CTX
    with type Address.t = CC.Address.t
     and type Amount.t = CC.Amount.t
     and type Ticket_id.t = CC.Ticket_id.t
