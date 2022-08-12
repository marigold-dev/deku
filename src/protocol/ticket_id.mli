type ticketer = private
  | Deku of Address.t
  | Tezos of Deku_tezos.Contract_hash.t

type ticket_id = private Ticket_id of { ticketer : ticketer; data : bytes }
and t = ticket_id [@@deriving eq, ord, yojson]

val make : Deku_tezos.Contract_hash.t -> bytes -> t
val mint_ticket : Address.t -> bytes -> t

val from_tezos_ticket :
  Deku_tezos.Tezos_ticket_id.t -> (t, [> `Ticket_from_implicit ]) result
