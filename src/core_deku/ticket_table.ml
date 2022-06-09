open Helpers

module Tickets = struct
  module Map = Map.Make_with_yojson (Ticket_id)
  type t = Amount.t Map.t [@@deriving yojson]
  let get_balance t ~ticket = Map.find_opt ticket t
  let of_seq = Map.of_seq

  let singleton ~ticket ~amount = Map.of_seq (Seq.return (ticket, amount))

  let merge t ~ticket ~amount =
    let merger option ~amount =
      let value =
        Option.fold ~none:amount ~some:(fun x -> Amount.(x + amount)) option
      in
      Some value in
    Map.update ticket (merger ~amount) t
  let get_and_remove t ~ticket ~to_take =
    let%ok amount =
      Map.find_opt ticket t |> Option.to_result ~none:`Insufficient_funds in
    let%assert () =
      (`Insufficient_funds, Amount.to_int amount >= Amount.to_int to_take) in
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
    Ok (to_take, map)

  let subtract t ~ticket ~to_withdraw =
    let%ok _, map = get_and_remove t ~ticket ~to_take:to_withdraw in
    Ok map
  let fetch t tickets =
    let fetched =
      Core.List.fold_result
        ~f:(fun (lst, m) (ticket, amount) ->
          let%ok amount_available, m =
            get_and_remove m ~ticket ~to_take:amount in
          Ok ((ticket, amount_available) :: lst, m))
        ~init:([], t) tickets in
    fetched
  let fold = Map.fold
  let empty = Map.empty
end
module Map = Map.Make_with_yojson (Address)

type t = Tickets.t Map.t [@@deriving yojson]
let empty = Map.empty

let balance t ~sender ~ticket =
  Map.find_opt sender t
  |> Option.map (Tickets.get_balance ~ticket)
  |> Option.join
let update_tickets t ~sender ~tickets =
  Map.add sender (Tickets.of_seq tickets) t

let update_or_create ~amount ~ticket = function
  | Some x -> Some (Tickets.merge ~amount ~ticket x)
  | None -> Some (Tickets.singleton ~amount ~ticket)
let deposit t ~destination ~ticket ~amount =
  Map.update destination (update_or_create ~amount ~ticket) t

let transfer t ~sender ~destination ~ticket ~amount =
  let%ok map =
    Map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds in
  let%ok amount, map = Tickets.get_and_remove ~ticket ~to_take:amount map in
  let t = Map.add sender map t in
  let t = Map.update destination (update_or_create ~amount ~ticket) t in
  Ok t

let withdraw t ~sender ~ticket ~amount =
  let%ok map =
    Map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds in
  let%ok map = Tickets.subtract ~ticket ~to_withdraw:amount map in
  let t = Map.add sender map t in
  Ok t

let take_tickets t ~sender ~tickets =
  let%ok map =
    Map.find_opt sender t |> Option.to_result ~none:`Insufficient_funds in
  let%ok tickets, map = Tickets.fetch map tickets in
  Ok (List.to_seq tickets, Map.add sender map t)

let tickets t ~sender =
  let map = Map.find_opt sender t |> Option.value ~default:Tickets.empty in
  let tickets =
    Tickets.fold
      (fun ticket amount acc -> Seq.cons (ticket, amount) acc)
      map Seq.empty in
  (tickets, Map.remove sender t)
