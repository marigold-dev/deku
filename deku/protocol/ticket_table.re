open Helpers;

module M = Helpers.Int64_map;

[@deriving yojson]
type handle = M.key;

[@deriving yojson]
type ticket_with_amount = {
  id: Ticket_id.t,
  amount: Amount.t,
};

[@deriving yojson]
type t = {
  next_key: M.key,
  map: M.t(ticket_with_amount),
};

let empty = {next_key: 0L, map: M.empty};

let create_ticket = id => {id, amount: Amount.zero};
let unsafe_create_ticket = (id, amount) => {id, amount};

let add = (ticket, {next_key, map}) => {
  let map = M.add(next_key, ticket, map);
  (next_key, {next_key: Int64.succ(next_key), map});
};
let add_empty = ticket_id => {
  add(create_ticket(ticket_id));
};

let find_opt = (handle, {next_key: _, map}) => {
  M.find_opt(handle, map);
};

let remove = (handle, {next_key, map}) => {
  let.ok ticket =
    find_opt(handle, {next_key, map})
    |> Option.to_result(~none=`Invalid_ticket);
  let table = {
    let map = M.remove(handle, map);
    {next_key, map};
  };
  Ok((ticket, table));
};

let recreate = (handle, table) => {
  let.ok (ticket, table) = remove(handle, table);
  let (handle, table) = add(ticket, table);
  Ok((handle, table));
};

type split = {
  split_at: handle,
  remaining: handle,
};

let split = (handle, ~at as split_at, table) => {
  let.ok (ticket, table) = remove(handle, table);
  let.ok (ticket_split_at, ticket_remaining) =
    if (ticket.amount >= split_at) {
      Ok((
        {id: ticket.id, amount: split_at},
        {id: ticket.id, amount: Amount.(ticket.amount - split_at)},
      ));
    } else {
      Error(`Not_enough_funds);
    };
  let (split_at, table) = add(ticket_split_at, table);
  let (remaining, table) = add(ticket_remaining, table);

  Ok(({split_at, remaining}, table));
};

let join = (handle_a, handle_b, table) => {
  let.ok (ticket_a, table) = remove(handle_a, table);
  let.ok (ticket_b, table) = remove(handle_b, table);
  if (Ticket_id.equal(ticket_a.id, ticket_b.id)) {
    let ticket = {
      id: ticket_a.id,
      amount: Amount.(ticket_a.amount + ticket_b.amount),
    };
    Ok(add(ticket, table));
  } else {
    Error(`Unlike_tickets);
  };
};
