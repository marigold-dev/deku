type t [@@deriving yojson]

val empty : t

val balance : t -> sender:Address.t -> ticket:Ticket_id.t -> Amount.t option

val deposit :
  t -> destination:Address.t -> ticket:Ticket_id.t -> amount:Amount.t -> t

val transfer :
  t ->
  sender:Address.t ->
  destination:Address.t ->
  ticket:Ticket_id.t ->
  amount:Amount.t ->
  (t, [> `Insufficient_funds]) result

val withdraw :
  t ->
  sender:Address.t ->
  ticket:Ticket_id.t ->
  amount:Amount.t ->
  (t, [> `Insufficient_funds]) result

val tickets : t -> sender:Address.t -> (Ticket_id.t * Amount.t) Seq.t * t

val take_tickets :
  t ->
  sender:Address.t ->
  tickets:(Ticket_id.t * Amount.t) list ->
  ((Ticket_id.t * Amount.t) Seq.t * t, [> `Insufficient_funds]) result

val update_tickets :
  t -> sender:Address.t -> tickets:(Ticket_id.t * Amount.t) Seq.t -> t
