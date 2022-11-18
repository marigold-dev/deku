open Deku_stdlib

exception
  Table of
    [ `Ownership_violation
    | `Invalid_split_amount
    | `Ticket_doesnt_exist
    | `Joined_different_tickets ]

let show_error = function
  | `Ownership_violation -> "Ownership violation"
  | `Invalid_split_amount -> "Split an invalid amount"
  | `Ticket_doesnt_exist -> "Ticket doesnt exist"
  | `Joined_different_tickets -> "Tried to join different tickets"

module Entry = struct
  open Deku_ledger
  open Deku_stdlib

  type t = { ticket_id : Ticket_id.t; amount : N.t; mutable alive : bool }
  [@@deriving ord, show]

  let assert_not_dead t =
    if t.alive = false then raise (Table `Ownership_violation) else ()

  let join t1 t2 =
    {
      ticket_id = t1.ticket_id;
      amount = N.(t1.amount + t2.amount);
      alive = true;
    }

  let split t (amount1, amount2) =
    let total_amount = N.(amount1 + amount2) in
    if not (N.equal t.amount total_amount) then
      raise (Table `Invalid_split_amount)
    else
      let ticket1 = { t with amount = amount1; alive = true } in
      let ticket2 = { t with amount = amount2; alive = true } in
      (ticket1, ticket2)
end

module Vec = struct
  module Table = Hashtbl.Make (struct
    type t = int

    let hash = Hashtbl.hash
    let equal = Int.equal
  end)

  type 'a t = { mutable counter : int; content : 'a Table.t }

  let reset t =
    t.counter <- 0;
    Table.clear t.content

  let[@inline always] add t content =
    let counter = t.counter in
    Table.replace t.content counter content;
    t.counter <- counter + 1;
    counter

  let init initial_values =
    let table = Table.create 200 in
    List.iteri (fun idx x -> Table.replace table idx x) initial_values;
    { counter = 0; content = table }

  let[@inline always] get t handle = Table.find_opt t.content handle
end

module Handle = struct
  type t = Vec.Table.key
end

type t = Entry.t Vec.t

let[@inline always] add t ticket = Vec.add t ticket

let extract t handle =
  let ticket : Entry.t option = Vec.get t handle in
  match ticket with
  | None -> raise (Table `Ticket_doesnt_exist)
  | Some x -> if not x.alive then raise (Table `Ownership_violation) else x

let unsafe_read t handle =
  let ticket : Entry.t option = Vec.get t handle in
  match ticket with
  | None -> raise (Table `Ticket_doesnt_exist)
  | Some x ->
      if not x.alive then raise (Table `Ownership_violation)
      else (
        x.alive <- false;
        x)

let mint_ticket t ticket_id amount =
  let entry = Entry.{ ticket_id; amount; alive = true } in
  add t entry

let read_ticket (t : t) handle =
  let ticket : Entry.t = unsafe_read t handle in
  let handle = add t { ticket with alive = true } in
  (ticket.ticket_id, ticket.amount, handle)

let split_ticket t handle (amount1, amount2) =
  let ticket = unsafe_read t handle in
  try
    let t1, t2 = Entry.split ticket (amount1, amount2) in
    let handle1 = add t t1 in
    let handle2 = add t t2 in
    Some (handle1, handle2)
  with Table _ -> None

let join_tickets t (handle1, handle2) =
  let ticket1 = unsafe_read t handle1 in
  let ticket2 = unsafe_read t handle2 in
  if Deku_ledger.Ticket_id.(compare ticket1.ticket_id ticket2.ticket_id <> 0)
  then None
  else
    let ticket = Entry.join ticket1 ticket2 in
    let handle = add t ticket in
    Some handle

let reset t = Vec.reset t
let init (initial : Entry.t list) = Vec.init initial
