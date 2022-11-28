type tezos_ticket_id =
  | Tezos_ticket_id of { ticketer : Tezos_contract.t; data : bytes }

and t = tezos_ticket_id [@@deriving show, eq, ord]

let deku_encoding =
  let open Data_encoding in
  conv
    (fun (Tezos_ticket_id { ticketer; data }) -> (ticketer, data))
    (fun (ticketer, data) -> Tezos_ticket_id { ticketer; data })
    (tup2 Tezos_contract.encoding bytes)
