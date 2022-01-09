module Handle: {
  [@deriving (eq, yojson)]
  type t;
  let to_string: t => string;
  let of_string: string => option(t);
};

[@deriving yojson]
type ticket_with_amount =
  pri {
    id: Ticket_id.t,
    amount: Amount.t,
  };

[@deriving yojson]
type t;

type split = {
  split_at: Handle.t,
  remaining: Handle.t,
};

let empty: t;
let unsafe_create_ticket: (Ticket_id.t, Amount.t) => ticket_with_amount;
let add_empty: (Ticket_id.t, t) => (handle, t);
let add: (ticket_with_amount, t) => (handle, t);
let find_opt: (handle, t) => option(ticket_with_amount);
let remove:
  (Handle.t, t) => result((ticket_with_amount, t), [> | `Invalid_ticket]);
let recreate: (Handle.t, t) => result((Handle.t, t), [> | `Invalid_ticket]);
let split:
  (Handle.t, ~at: Amount.t, t) =>
  result((split, t), [> | `Invalid_ticket | `Not_enough_funds]);
let join:
  (Handle.t, Handle.t, t) =>
  result((Handle.t, t), [> | `Invalid_ticket | `Unlike_tickets]);
let get_id: (Handle.t, t) => option(Ticket_id.t);
