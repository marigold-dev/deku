/**

## What is this module for?

This solves a problem related to the way contracts manipulate
tickets. Contracts treat tickets like data. You send a ticket to
a contract by invoking the contract and passing the ticket
as a parameter, which then stores them in the contract data
storage, etc. It's very important that a contract not be able to
recieve a ticket and send it twice.

We prevent this by representing tickets as *ticket handles*.
When you receive a ticket, you can call a zinc instruction that
invalidates the old handle and gives you a new one. (In the
future maybe this instruction can be added automatically by the
ligo->zinc compiler.) If the ticket has already been invalidated,
the instruction will fail, and the everything will be rolled back.

This module handles automatically creating and invalidating these
handles. Here's an example of how the own instruction might
be implemented:

let own = (handle, table) => {
  let.ok (ticket, table) = remove(handle, table);
  let (handle, table) = add(ticket, table);
  Ok((handle, table));
};

`remove` takes a handle and removes the corresponding ticket
from the table and returns it. And `add` adds a ticket to the
table, returning a new handle. The way the API is designed makes
it difficult to write a buggy version of this function that
doesn't remove the original ticket, because the only way to get
the ticket that corresponds to a handle is by calling `remove`.
And `remove` also returns a version of the table without that
ticket. You could discard the table it returns, but don't do that.

*/

[@deriving yojson]
type handle;

[@deriving yojson]
type ticket_with_amount =
  pri {
    id: Ticket_id.t,
    amount: Amount.t,
  };

[@deriving yojson]
type t;

type split = {
  split_at: handle,
  remaining: handle,
};

let empty: t;
let unsafe_create_ticket: (Ticket_id.t, Amount.t) => ticket_with_amount;
let add_empty: (Ticket_id.t, t) => (handle, t);
let add: (ticket_with_amount, t) => (handle, t);
let find_opt: (handle, t) => option(ticket_with_amount);
let remove:
  (handle, t) => result((ticket_with_amount, t), [> | `Invalid_ticket]);
let split:
  (handle, ~at: Amount.t, t) =>
  result((split, t), [> | `Invalid_ticket | `Not_enough_funds]);
let join:
  (handle, handle, t) =>
  result((handle, t), [> | `Invalid_ticket | `Unlike_tickets]);
