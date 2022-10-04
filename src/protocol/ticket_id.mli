type ticketer =
  | Tezos of Deku_tezos.Contract_hash.t
  | Deku of Contract_address.t
[@@deriving eq, ord, show]

type ticket_id = private Ticket_id of { ticketer : ticketer; data : bytes }
[@@deriving show, ord]

and t = ticket_id [@@deriving eq, ord, yojson]

val make : ticketer -> bytes -> t

val from_tezos_ticket :
  Deku_tezos.Ticket_id.t -> (t, [> `Ticket_from_implicit ]) result

val to_tezos_ticket : t -> Deku_tezos.Ticket_id.t option
val to_string : t -> string
val of_string : string -> (t, [> `Ticket_from_implicit | `Cannot_parse ]) result
