open Deku_concepts
open Deku_stdlib

module Tickets = struct
  module Ticket_map = Map.Make (Ticket_id)

  type t = Amount.t Ticket_map.t [@@deriving yojson]

  let encodings = Ticket_map.encoding Amount.encoding
  let get_balance ~ticket_id t = Ticket_map.find_opt ticket_id t
  let to_seq map = Ticket_map.to_seq map
  let of_seq seq = Ticket_map.of_seq seq

  let singleton ~ticket_id ~amount =
    Ticket_map.of_seq (Seq.return (ticket_id, amount))

  let join t ~ticket_id ~amount =
    let join option ~amount =
      let value = Option.fold ~none:amount ~some:Amount.(( + ) amount) option in
      value
    in
    Ticket_map.update ticket_id
      (fun x ->
        let joined = join ~amount x in
        Option.some joined)
      t

  let get_and_remove ~ticket_id ~to_take t =
    match
      Ticket_map.update ticket_id
        (function
          | Some amount ->
              let () =
                let compared = Amount.compare amount to_take in
                if compared >= 0 then () else failwith "error"
              in
              Amount.(amount - to_take)
          | None -> failwith "error")
        t
    with
    | _ as x -> Ok x
    | exception Failure _ -> Error `Insufficient_funds

  let get_and_remove_many tickets t =
    let fetched =
      List.fold_left_ok
        (fun (lst, m) (ticket_id, amount) ->
          let%ok m = get_and_remove ~ticket_id ~to_take:amount m in
          let lst = Seq.cons (ticket_id, amount) lst in
          Ok (lst, m))
        (Seq.empty, t) tickets
    in
    fetched

  let empty = Ticket_map.empty
  let is_empty = Ticket_map.is_empty
end

module Ticket_map = Tickets.Ticket_map
module Address_map = Map.Make (Address)

type t = Tickets.t Address_map.t [@@deriving yojson]

let encoding = Address_map.encoding Tickets.encodings
let empty = Address_map.empty

let balance ~sender ~ticket_id t =
  let%some map = Address_map.find_opt sender t in
  Tickets.get_balance ~ticket_id map

let balances_all_tickets ~sender t =
  let map =
    Address_map.find_opt sender t |> Option.value ~default:Tickets.empty
  in
  Tickets.to_seq map

let update_or_create ~amount ~ticket_id t =
  Option.fold
    ~some:(fun x -> Tickets.join ~amount ~ticket_id x)
    ~none:(Tickets.singleton ~amount ~ticket_id)
    t
  |> Option.some

let update_tickets ~sender ~ticket_ids t =
  if Seq.is_empty ticket_ids then Address_map.remove sender t
  else Address_map.add sender (Tickets.of_seq ticket_ids) t

let deposit ~destination ~ticket_id ~amount t =
  Address_map.update destination (update_or_create ~amount ~ticket_id) t

let update ~sender ~submap t =
  if Tickets.is_empty submap then Address_map.remove sender t
  else Address_map.add sender submap t

let transfer ~sender ~receiver ~ticket_id ~amount t =
  let%ok map =
    Address_map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds
  in
  let%ok map = Tickets.get_and_remove ~ticket_id ~to_take:amount map in
  let t = update t ~sender ~submap:map in
  let t = Address_map.update receiver (update_or_create ~amount ~ticket_id) t in
  Ok t

let withdraw ~sender ~ticket_id ~amount t =
  let%ok map =
    Address_map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds
  in
  let%ok map = Tickets.get_and_remove ~ticket_id ~to_take:amount map in
  let t = update t ~sender ~submap:map in
  Ok t

let take_tickets ~sender ~ticket_ids
    (t : Amount.t Tickets.Ticket_map.t Address_map.t) =
  let%ok map =
    Address_map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds
  in
  let%ok ticket_ids, map = Tickets.get_and_remove_many ticket_ids map in
  let t = update ~sender ~submap:map t in
  Ok (ticket_ids, t)

let take_all_tickets ~sender t =
  let tickets = balances_all_tickets ~sender t in
  (tickets, Address_map.remove sender t)
