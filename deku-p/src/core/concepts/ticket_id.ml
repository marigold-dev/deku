open Deku_stdlib
open Deku_tezos

type ticket_id = Ticket_id of { ticketer : Contract.t; data : bytes }
and t = ticket_id [@@deriving show, eq, ord]

let mint_deku_ticket ~ticketer ~data =
  let ticketer = Contract.of_deku_contract_address ticketer in
  Ticket_id { ticketer; data }

let encode_tezos_ticket_id ~deku_ticketer ticket_id =
  let open Tezos_ticket_id in
  let open Contract in
  let (Ticket_id { ticketer; data }) = ticket_id in
  match ticketer with
  | Contract_implicit ticketer ->
      let ticketer = Tezos_contract.of_key_hash ticketer in
      Tezos_ticket_id { ticketer; data }
  | Contract_tezos_originated tezos_contract_hash ->
      let ticketer =
        Tezos_contract.of_tezos_contract_hash tezos_contract_hash
      in
      Tezos_ticket_id { ticketer; data }
  | Contract_deku_originated deku_contract_address ->
      let ticketer = Tezos_contract.of_tezos_contract_hash deku_ticketer in
      let data =
        let ticketer =
          Contract.of_deku_contract_address deku_contract_address
        in
        let ticketer =
          Data_encoding.Binary.to_bytes_exn Contract.encoding ticketer
        in
        Tezos_deku.Ticket_data.encode ~ticketer ~data
      in
      Tezos_ticket_id { ticketer; data }

let decode_tezos_ticket_id ~deku_ticketer tezos_ticket_id =
  let open Tezos_contract in
  let open Tezos_ticket_id in
  let (Tezos_ticket_id { ticketer; data }) = tezos_ticket_id in
  match ticketer with
  | Tezos_contract_implicit ticketer ->
      let ticketer = Contract.of_key_hash ticketer in
      Some (Ticket_id { ticketer; data })
  | Tezos_contract_originated ticketer -> (
      match Tezos_contract_hash.equal deku_ticketer ticketer with
      | true ->
          let%some ticketer, data = Tezos_deku.Ticket_data.decode data in
          let%some ticketer =
            Data_encoding.Binary.of_bytes_opt Contract.encoding ticketer
          in
          Some (Ticket_id { ticketer; data })
      | false ->
          let ticketer = Contract.of_tezos_contract_hash ticketer in
          Some (Ticket_id { ticketer; data }))

let encoding =
  let open Data_encoding in
  conv
    (fun (Ticket_id { ticketer; data }) -> (ticketer, data))
    (fun (ticketer, data) -> Ticket_id { ticketer; data })
    (tup2 Contract.encoding bytes)
