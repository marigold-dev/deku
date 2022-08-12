module type S = sig
  include Conversions.S

  type t =
    | Transfer of {
        destination : Address.t;
        ticket : Ticket_id.t;
        amount : Amount.t;
      }
    | Invoke of {
        param : bytes;
        destination : Address.t;
        tickets :
          ((Ticket_id.t * Amount.t) * (Ticket_handle.t * Int64.t option)) list;
      }
end

module Make (CC : Conversions.S) = struct
  include CC

  type t =
    | Transfer of {
        destination : Address.t;
        ticket : Ticket_id.t;
        amount : Amount.t;
      }
    | Invoke of {
        param : bytes;
        destination : Address.t;
        tickets :
          ((Ticket_id.t * Amount.t) * (Ticket_handle.t * Int64.t option)) list;
      }
end
