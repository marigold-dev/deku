module Make (CC : Conversions.S) :
  Context.CTX
    with module Address = CC.Address
     and module Ticket_id = CC.Ticket_id
     and module Amount = CC.Amount
