open Deku_concepts
open Deku_stdlib

type ledger = Ledger of { table : Ticket_table.t }
and t = ledger

let initial = Ledger { table = Ticket_table.empty }

let balance address ticket_id (Ledger { table }) =
  Ticket_table.balance ~sender:address ~ticket_id table
  |> Option.value ~default:Amount.zero

let deposit destination amount ticket_id (Ledger { table }) =
  let table = Ticket_table.deposit ~destination ~ticket_id ~amount table in
  Ledger { table }

let transfer ~sender ~receiver ~amount ~ticket_id (Ledger { table }) =
  let%ok table =
    Ticket_table.transfer table ~sender ~receiver ~amount ~ticket_id
    |> Result.map_error (fun _ -> `Insufficient_funds)
  in
  Ok (Ledger { table })
