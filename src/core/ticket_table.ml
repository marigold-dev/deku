open Helpers
module M = Helpers.Int64_map
type handle = M.key [@@deriving yojson]
type ticket_with_amount = {
  id : Ticket_id.t;
  amount : Amount.t;
}
[@@deriving yojson]
type t = {
  next_key : M.key;
  map : ticket_with_amount M.t;
}
[@@deriving yojson]
let empty = { next_key = 0L; map = M.empty }
let create_ticket id = { id; amount = Amount.zero }
let unsafe_create_ticket id amount = { id; amount }
let add ticket { next_key; map } =
  let map = M.add next_key ticket map in
  (next_key, { next_key = Int64.succ next_key; map })
let add_empty ticket_id = add (create_ticket ticket_id)
let find_opt handle { next_key = _; map } = M.find_opt handle map
let remove handle { next_key; map } =
  let%ok ticket =
    find_opt handle { next_key; map } |> Option.to_result ~none:`Invalid_ticket
  in
  let table =
    let map = M.remove handle map in
    { next_key; map } in
  Ok (ticket, table)
type split = {
  split_at : handle;
  remaining : handle;
}
let split handle ~at:split_at table =
  let%ok ticket, table = remove handle table in
  let%ok ticket_split_at, ticket_remaining =
    if ticket.amount >= split_at then
      Ok
        ( { id = ticket.id; amount = split_at },
          {
            id = ticket.id;
            amount =
              (let open Amount in
              ticket.amount - split_at);
          } )
    else
      Error `Not_enough_funds in
  let split_at, table = add ticket_split_at table in
  let remaining, table = add ticket_remaining table in
  Ok ({ split_at; remaining }, table)
let join handle_a handle_b table =
  let%ok ticket_a, table = remove handle_a table in
  let%ok ticket_b, table = remove handle_b table in
  if Ticket_id.equal ticket_a.id ticket_b.id then
    let ticket =
      {
        id = ticket_a.id;
        amount =
          (let open Amount in
          ticket_a.amount + ticket_b.amount);
      } in
    Ok (add ticket table)
  else
    Error `Unlike_tickets
