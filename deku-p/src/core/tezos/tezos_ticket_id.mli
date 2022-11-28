type tezos_ticket_id =
  | Tezos_ticket_id of { ticketer : Tezos_contract.t; data : bytes }

type t = tezos_ticket_id [@@deriving show, eq, ord]

val deku_encoding : tezos_ticket_id Data_encoding.t
