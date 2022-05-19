
(*************************************************************************)
(* Ticket *)

let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core.Ticket_id.{ ticketer; data = Bytes.empty }
