type ticket_id =
  | Ticket_id of { ticketer : Deku_tezos.Contract_hash.t; data : bytes }

and t = ticket_id [@@deriving eq, ord, yojson]

let make ticketer data = Ticket_id { ticketer; data }

let from_tezos_ticket tezos_ticket =
  let Deku_tezos.Ticket_id.{ ticketer; data } = tezos_ticket in
  match ticketer with
  | Deku_tezos.Address.Implicit _address -> Error `Ticket_from_implicit
  | Deku_tezos.Address.Originated { contract; _ } ->
      Ok (Ticket_id { ticketer = contract; data })

(* TODO: add Deku tickets back from v0 *)
