open Helpers
module Map = Ticket_handle.Map
module Set = Ticket_handle.Set

module Errors = struct
  type t =
    [ `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount
    | `Attempted_to_merge_different_tickets ]
  [@@deriving show]
  let doesnt_exist = Error `Ticket_doesnt_exist
  let insufficient_funds = Error `Insufficient_funds

  let ownership_violation = Error `Ticket_ownership_violation
  let different_tickets = Error `Attempted_to_merge_different_tickets

  let ticket_split_error = Error `Ticket_split_invalid_amount
end

open struct
  let ok = Ok ()
  let assert_ownership sender ~ticket ~ticket_handle =
    let checked_handle = Ticket_handle.make sender ticket in
    if Ticket_handle.equal checked_handle ticket_handle then
      ok
    else
      Errors.ownership_violation

  let assert_enough_funds ~ticket_total ~amount =
    if Amount.(compare ticket_total amount = -1) then
      Errors.insufficient_funds
    else
      ok
  let assert_same_amount ticket_total amount =
    if Amount.(equal ticket_total amount) then
      Ok ()
    else
      Errors.ticket_split_error

  let assert_same_tickets t1 t2 =
    if Ticket_id.equal t1 t2 then ok else Errors.different_tickets
end

module Ticket_repr = struct
  type t = {
    ticket : Ticket_id.t;
    amount : Amount.t;
  }
  [@@deriving yojson, eq, ord]
  let[@inline] ticket t = t.ticket

  let make ticket amount = { ticket; amount }
  let join t1 t2 =
    let%ok () = assert_same_tickets t1.ticket t2.ticket in
    Ok { t1 with amount = Amount.(t1.amount + t2.amount) }

  let split ticket ~ticket_total ~amounts:(first, second) =
    let total_amount = Amount.(first + second) in
    let%ok () = assert_same_amount ticket_total total_amount in
    let%ok () = assert_enough_funds ~ticket_total ~amount:total_amount in
    let ticket1 = { ticket; amount = first } in
    let ticket2 = { ticket; amount = second } in
    Ok (ticket1, ticket2)
end
type t = {
  global : Ticket_repr.t Ticket_handle.Map.t;
  temp : Ticket_handle.Set.t;
      (* we use temp during execution to drop tickets that are no longer owned by anyone *)
}
[@@deriving yojson]

let empty = { global = Ticket_handle.Map.empty; temp = Ticket_handle.Set.empty }
let validate (t : t) : t =
  let global =
    Ticket_handle.Set.fold (fun x acc -> Map.remove x acc) t.temp t.global in
  { global; temp = Ticket_handle.Set.empty }

let amount t address ticket =
  let handle = Ticket_handle.make address ticket in
  Map.find_opt handle t.global |> Option.map (fun x -> x.Ticket_repr.amount)

let unsafe_read t ~handle =
  Map.find_opt handle t |> Option.map (fun x -> (x, Map.remove handle t))

let unsafe_deposit_ticket { global; temp } ~ticket ~destination ~amount =
  let handle = Ticket_handle.make destination ticket in
  let new_ticket = Ticket_repr.make ticket amount in
  let state = unsafe_read global ~handle in
  {
    global =
      (match state with
      | Some (existing_ticket, t) ->
        let joined =
          Ticket_repr.join existing_ticket new_ticket |> Result.get_ok in
        (* error stemming from Result.get_ok should never occur  *)
        Map.add handle joined t
      | None -> Map.add handle new_ticket global);
    temp;
  }
let own t sender ticket_handle =
  let%assert () = (`Ticket_ownership_violation, Set.mem ticket_handle t.temp) in
  let%ok ticket, global =
    match unsafe_read t.global ~handle:ticket_handle with
    | Some (ticket, global) -> Ok (ticket, global)
    | None -> Errors.doesnt_exist in
  let temp = Set.remove ticket_handle t.temp in
  let handle = Ticket_handle.make sender ticket.ticket in
  Ok
    ( handle,
      {
        global =
          Map.update handle
            (function
              | Some existing ->
                let joined = Ticket_repr.join ticket existing |> Result.get_ok in
                (* error stemming from Result.get_ok should never occur  *)
                Some joined
              | None -> Some ticket)
            global;
        temp;
      } )

let read_ticket t ~sender ~ticket_handle =
  Map.find_opt ticket_handle t.global
  |> Option.fold
       ~some:(fun { Ticket_repr.ticket; amount } ->
         let%ok () = assert_ownership sender ~ticket ~ticket_handle in
         let global = Map.remove ticket_handle t.global in
         let handle = Ticket_handle.make_temp sender ticket amount in
         let to_return =
           ( (ticket, amount, handle),
             {
               global = Map.add handle { Ticket_repr.ticket; amount } global;
               temp = Set.add handle t.temp;
             } ) in
         Ok to_return)
       ~none:Errors.doesnt_exist

let disown t sender ticket_handle =
  let%ok (ticket_id, amount, _), t = read_ticket t ~sender ~ticket_handle in
  let handle = Ticket_handle.make_temp sender ticket_id amount in
  let temp = Set.add handle t.temp in
  let global = Map.add handle (Ticket_repr.make ticket_id amount) t.global in
  Ok { global; temp }

let split_ticket t ~sender ~ticket_handle ~amounts =
  let%ok (ticket, amount, _), t = read_ticket t ~sender ~ticket_handle in
  let%ok handle, handle2 =
    Ticket_repr.split ticket ~ticket_total:amount ~amounts in
  let handle1 = Ticket_handle.make_temp sender handle.ticket handle.amount in
  let temp = Set.add handle1 t.temp in
  let global =
    Map.update handle1
      (function
        | Some existing ->
          let joined = Ticket_repr.join handle existing |> Result.get_ok in
          (* error stemming from Result.get_ok should never occur  *)
          Some joined
        | None -> Some handle)
      t.global in
  let handle2 = Ticket_handle.make_temp sender handle2.ticket handle2.amount in
  let temp = Set.add handle2 temp in
  let global =
    Map.update handle2
      (function
        | Some existing ->
          let joined = Ticket_repr.join handle existing |> Result.get_ok in
          (* error stemming from Result.get_ok should never occur  *)
          Some joined
        | None -> Some handle)
      global in
  Ok ((handle1, handle2), { global; temp })

let join_tickets t ~source ~senders:(sender1, sender2)
    ~handles:(handle1, handle2) =
  let%ok (ticket, amount, _), t =
    read_ticket t ~sender:sender1 ~ticket_handle:handle1 in
  let%ok (ticket2, amount2, _), t =
    read_ticket t ~sender:sender2 ~ticket_handle:handle2 in
  let%ok ticket =
    Ticket_repr.join { ticket; amount } { ticket = ticket2; amount = amount2 }
  in
  let handle1 = Ticket_handle.make_temp source ticket.ticket ticket.amount in
  let temp = Set.add handle1 t.temp in
  let global =
    Map.update handle1
      (function
        | Some existing ->
          let joined = Ticket_repr.join ticket existing |> Result.get_ok in
          (* error stemming from Result.get_ok should never occur  *)
          Some joined
        | None -> Some ticket)
      t.global in
  Ok (handle1, { global; temp })

let transfer t ~sender ~ticket ~destination ~amount =
  let ticket_handle = Ticket_handle.make sender ticket in
  let%ok (ticket, ticket_total, _), t = read_ticket t ~sender ~ticket_handle in
  let%ok () = assert_enough_funds ~ticket_total ~amount in
  let%ok to_send, to_hold =
    Ticket_repr.split ticket ~ticket_total
      ~amounts:Amount.(amount, ticket_total - amount) in
  let t =
    unsafe_deposit_ticket t ~ticket:to_send.ticket ~amount:to_send.amount
      ~destination in
  if Amount.(equal ticket_total amount) then
    Ok (t |> validate)
  else
    Ok
      (unsafe_deposit_ticket t ~ticket:to_hold.ticket ~amount:to_hold.amount
         ~destination:sender
      |> validate)

let unsafe_withdraw t ~ticket ~sender ~amount =
  let handle = Ticket_handle.make sender ticket in
  let%ok (ticket_id, total_amount, _), t =
    read_ticket t ~sender ~ticket_handle:handle in
  let%ok () = assert_enough_funds ~ticket_total:total_amount ~amount in
  let%ok _, handle2 =
    Ticket_repr.split ticket_id ~ticket_total:total_amount
      ~amounts:Amount.(amount, total_amount - amount) in
  Ok
    (unsafe_deposit_ticket t ~ticket:handle2.ticket ~destination:sender
       ~amount:handle2.amount
    |> validate)
let add_to_temporary t tickets =
  { global = t.global; temp = Set.of_list tickets }
