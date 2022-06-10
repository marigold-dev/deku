open Helpers

module Tickets = struct
  module Map = Map.Make_with_yojson (Ticket_id)

  type t = Amount.t Map.t [@@deriving yojson]

  let get_balance t ~ticket = Map.find_opt ticket t

  let of_seq = Map.of_seq

  let singleton ~ticket ~amount = Map.of_seq (Seq.return (ticket, amount))

  let merge t ~ticket ~amount =
    let merger option ~amount =
      let value = Option.fold ~none:amount ~some:Amount.(( + ) amount) option in
      value in
    Map.update ticket
      (fun x ->
        let merged = merger ~amount x in
        Option.some merged)
      t

  let get_and_remove t ~ticket ~to_take =
    let%ok amount =
      Map.find_opt ticket t |> Option.to_result ~none:`Insufficient_funds in
    let%assert () =
      ( `Insufficient_funds,
        let compared = Amount.compare amount to_take in
        compared >= 0 ) in
    let map =
      Map.update ticket
        (function
          | Some amount ->
            if Amount.equal amount to_take then
              None
            else
              Some Amount.(amount - to_take)
          | None -> assert false)
        t in
    Ok map

  let subtract t ~ticket ~to_withdraw =
    let%ok map = get_and_remove t ~ticket ~to_take:to_withdraw in
    Ok map

  let fetch t tickets =
    let fetched =
      List.fold_left_ok
        (fun (lst, m) (ticket, amount) ->
          let%ok m = get_and_remove m ~ticket ~to_take:amount in
          let lst = Seq.cons (ticket, amount) lst in
          Ok (lst, m))
        (Seq.empty, t) tickets in
    fetched

  let fold = Map.fold

  let empty = Map.empty

  let is_empty = Map.is_empty
end

module Table = Map.Make_with_yojson (Address)

type t = Tickets.t Table.t [@@deriving yojson]

let empty = Table.empty

let balance t ~sender ~ticket =
  let%some map = Table.find_opt sender t in
  Tickets.get_balance ~ticket map

let update_tickets t ~sender ~tickets =
  if Seq.is_empty tickets then
    Table.remove sender t
  else
    Table.add sender (Tickets.of_seq tickets) t

let update_or_create ~amount ~ticket t =
  Option.fold
    ~some:(fun x -> Tickets.merge ~amount ~ticket x)
    ~none:(Tickets.singleton ~amount ~ticket)
    t
  |> Option.some

let deposit t ~destination ~ticket ~amount =
  Table.update destination (update_or_create ~amount ~ticket) t

let remove_if_empty t ~sender ~submap =
  if Tickets.is_empty submap then
    Table.remove sender t
  else
    Table.add sender submap t

let transfer t ~sender ~destination ~ticket ~amount =
  let%ok map =
    Table.find_opt sender t |> Option.to_result ~none:`Insufficient_funds in
  let%ok map = Tickets.get_and_remove ~ticket ~to_take:amount map in
  let t = remove_if_empty t ~sender ~submap:map in
  let t = Table.update destination (update_or_create ~amount ~ticket) t in
  Ok t

let withdraw t ~sender ~ticket ~amount =
  let%ok map =
    Table.find_opt sender t |> Option.to_result ~none:`Insufficient_funds in
  let%ok map = Tickets.subtract ~ticket ~to_withdraw:amount map in
  let t = remove_if_empty t ~sender ~submap:map in
  Ok t

let take_tickets t ~sender ~tickets =
  let%ok map =
    Table.find_opt sender t |> Option.to_result ~none:`Insufficient_funds in
  let%ok tickets, map = Tickets.fetch map tickets in
  let t = remove_if_empty t ~sender ~submap:map in
  Ok (tickets, t)

let tickets t ~sender =
  let map = Table.find_opt sender t |> Option.value ~default:Tickets.empty in
  let tickets =
    Tickets.fold
      (fun ticket amount acc -> Seq.cons (ticket, amount) acc)
      map Seq.empty in
  (tickets, Table.remove sender t)
