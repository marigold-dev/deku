open Deku_concepts
open Deku_stdlib
module Ticket_map : Map.S with type key = Ticket_id.t
module Address_map : Map.S with type key = Address.t

type t = private Amount.t Ticket_map.t Address_map.t [@@deriving yojson]

val empty : t
val balance : sender:Address.t -> ticket_id:Ticket_id.t -> t -> Amount.t option

val deposit :
  destination:Address.t -> ticket_id:Ticket_id.t -> amount:Amount.t -> t -> t

val transfer :
  sender:Address.t ->
  receiver:Address.t ->
  ticket_id:Ticket_id.t ->
  amount:Amount.t ->
  t ->
  (t, [> `Insufficient_funds ]) result

val withdraw :
  sender:Address.t ->
  ticket_id:Ticket_id.t ->
  amount:Amount.t ->
  t ->
  (t, [> `Insufficient_funds | `Withdraw_zero_ticket ]) result

val take_tickets :
  sender:Address.t ->
  ticket_ids:(Ticket_id.t * Amount.t) list ->
  t ->
  ((Ticket_id.t * Amount.t) Seq.t * t, [> `Insufficient_funds ]) result

val take_all_tickets :
  sender:Address.t -> t -> (Ticket_id.t * Amount.t) Seq.t * t

val update_tickets :
  sender:Address.t -> ticket_ids:(Ticket_id.t * Amount.t) Seq.t -> t -> t
