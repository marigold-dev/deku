[@deriving yojson]
type handle;

[@deriving yojson]
type ticket_with_amount =
  pri {
    id: Ticket.t,
    amount: Amount.t,
  };

[@deriving yojson]
type t;

type split = {
  split_at: handle,
  remaining: handle,
};

let empty: t;
let unsafe_create_ticket: (Ticket.t, Amount.t) => ticket_with_amount;
let add_empty: (Ticket.t, t) => (handle, t);
let add: (ticket_with_amount, t) => (handle, t);
let find_opt: (handle, t) => option(ticket_with_amount);
let remove:
  (handle, t) => result((ticket_with_amount, t), [> | `Invalid_ticket]);
let recreate: (handle, t) => result((handle, t), [> | `Invalid_ticket]);
let split:
  (handle, ~at: Amount.t, t) =>
  result((split, t), [> | `Invalid_ticket | `Not_enough_funds]);
let join:
  (handle, handle, t) =>
  result((handle, t), [> | `Invalid_ticket | `Unlike_tickets]);
