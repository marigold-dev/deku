type t =
  | Transfer of {
      destination : Address.t;
      ticket : Ticket_id.t;
      amount : Amount.t;
    }
  | Invoke   of {
      param : bytes;
      destination : Address.t;
      tickets : (Ticket_id.t * Amount.t) list;
    }
