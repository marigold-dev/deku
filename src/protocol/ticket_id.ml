type ticketer = Deku of Address.t | Tezos of Deku_tezos.Contract_hash.t
[@@deriving eq, ord, yojson]

type ticket_id = Ticket_id of { ticketer : ticketer; data : bytes }
and t = ticket_id [@@deriving eq, ord, yojson]

let make ticketer data = Ticket_id { ticketer = Tezos ticketer; data }
let mint_ticket ticketer data = Ticket_id { ticketer = Deku ticketer; data }

let from_tezos_ticket tezos_ticket =
  let Deku_tezos.Tezos_ticket_id.{ ticketer; data } = tezos_ticket in
  match ticketer with
  | Deku_tezos.Address.Implicit _address -> Error `Ticket_from_implicit
  | Deku_tezos.Address.Originated { contract; _ } ->
      Ok (Ticket_id { ticketer = Tezos contract; data })

(* TODO: add Deku tickets back from v0 *)
