open Helpers

module Tickets = struct
  module Ticket_map = Map.Make_with_yojson (Ticket_id)

  type t = Amount.t Ticket_map.t [@@deriving yojson]

  let get_balance t ~ticket = Ticket_map.find_opt ticket t

  let to_seq map = Ticket_map.to_seq map

  let of_seq seq = Ticket_map.of_seq seq

  let singleton ~ticket ~amount =
    Ticket_map.of_seq (Seq.return (ticket, amount))

  let join t ~ticket ~amount =
    let join option ~amount =
      let value = Option.fold ~none:amount ~some:Amount.(( + ) amount) option in
      value in
    Ticket_map.update ticket
      (fun x ->
        let joined = join ~amount x in
        Option.some joined)
      t

  let get_and_remove t ~ticket ~to_take =
    match
      Ticket_map.update ticket
        (function
          | Some amount ->
            let () =
              let compared = Amount.compare amount to_take in
              if compared >= 0 then () else failwith "error" in
            if Amount.equal amount to_take then
              None
            else
              Some Amount.(amount - to_take)
          | None -> failwith "error")
        t
    with
    | _ as x -> Ok x
    | exception Failure _ -> Error `Insufficient_funds

  let get_and_remove_many t tickets =
    let fetched =
      List.fold_left_ok
        (fun (lst, m) (ticket, amount) ->
          let%ok m = get_and_remove m ~ticket ~to_take:amount in
          let lst = Seq.cons (ticket, amount) lst in
          Ok (lst, m))
        (Seq.empty, t) tickets in
    fetched

  let empty = Ticket_map.empty

  let is_empty = Ticket_map.is_empty
end

module Address_map = Map.Make_with_yojson (Address)

type t = Tickets.t Address_map.t [@@deriving yojson]

let empty = Address_map.empty

let balance t ~sender ~ticket =
  let%some map = Address_map.find_opt sender t in
  Tickets.get_balance ~ticket map

let update_or_create ~amount ~ticket t =
  Option.fold
    ~some:(fun x -> Tickets.join ~amount ~ticket x)
    ~none:(Tickets.singleton ~amount ~ticket)
    t
  |> Option.some

let update_tickets t ~sender ~tickets =
  if Seq.is_empty tickets then
    Address_map.remove sender t
  else
    Address_map.add sender (Tickets.of_seq tickets) t

let deposit t ~destination ~ticket ~amount =
  Address_map.update destination (update_or_create ~amount ~ticket) t

let update t ~sender ~submap =
  if Tickets.is_empty submap then
    Address_map.remove sender t
  else
    Address_map.add sender submap t

let transfer t ~sender ~destination ~ticket ~amount =
  let%ok map =
    Address_map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds
  in
  let%ok map = Tickets.get_and_remove ~ticket ~to_take:amount map in
  let t = update t ~sender ~submap:map in
  let t = Address_map.update destination (update_or_create ~amount ~ticket) t in
  Ok t

let withdraw t ~sender ~ticket ~amount =
  let%ok map =
    Address_map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds
  in
  let%ok map = Tickets.get_and_remove ~ticket ~to_take:amount map in
  let t = update t ~sender ~submap:map in
  Ok t

let take_tickets t ~sender ~tickets =
  let%ok map =
    Address_map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds
  in
  let%ok tickets, map = Tickets.get_and_remove_many map tickets in
  let t = update t ~sender ~submap:map in
  Ok (tickets, t)

let take_all_tickets t ~sender =
  let map =
    Address_map.find_opt sender t |> Option.value ~default:Tickets.empty in
  let tickets = Tickets.to_seq map in
  (tickets, Address_map.remove sender t)

let tickets t address =
  match Address_map.find_opt address t with
  | Some tickets -> tickets |> Tickets.to_seq |> List.of_seq
  | None -> []
