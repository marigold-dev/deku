type ticket_id = private
  | Ticket_id of { ticketer : Deku_tezos.Contract_hash.t; data : bytes }

and t = ticket_id [@@deriving eq, ord, yojson]

val make : Deku_tezos.Contract_hash.t -> bytes -> t

val from_tezos_ticket :
  Deku_tezos.Ticket_id.t -> (t, [> `Ticket_from_implicit ]) result

val to_string : t -> string
val of_string : string -> (t, [> `Ticket_from_implicit | `Cannot_parse ]) result
