open Deku_tezos

type ticket_id = private Ticket_id of { ticketer : Contract.t; data : bytes }
and t = ticket_id [@@deriving show, eq, ord]

val mint_deku_ticket :
  ticketer:Deku_contract_address.t -> data:bytes -> ticket_id

val encode_tezos_ticket_id :
  deku_ticketer:Tezos_contract_hash.t -> ticket_id -> Tezos_ticket_id.t

val decode_tezos_ticket_id :
  deku_ticketer:Tezos_contract_hash.t -> Tezos_ticket_id.t -> ticket_id option

(* repr *)
val encoding : ticket_id Data_encoding.t
