module type S = sig
  include Conversions.S

  type t =
    | Transfer of {
        destination : Address.t;
        ticket : Ticket_id.t;
        amount : Amount.t;
      }
    | Invoke   of {
        param : bytes;
        destination : Address.t;
        tickets :
          ((Ticket_id.t * Amount.t) * (Ticket_handle.t * Int64.t option)) list;
            (* TODO: CLEANUP, Int64.t is optional pointer in case of need to reassign handle *)
      }
end

module Make (CC : Conversions.S) :
  S
    with type Address.t = CC.Address.t
     and type Amount.t = CC.Amount.t
     and type Ticket_id.t = CC.Ticket_id.t
