open Deku_concepts

type ledger = private Ledger of { table : Ticket_table.t }
type t = ledger

val initial : t
val balance : Address.t -> Ticket_id.t -> t -> Amount.t
val deposit : Address.t -> Amount.t -> Ticket_id.t -> t -> t

val transfer :
  sender:Address.t ->
  receiver:Address.t ->
  amount:Amount.t ->
  ticket_id:Ticket_id.t ->
  t ->
  (t, [> `Insufficient_funds ]) result
