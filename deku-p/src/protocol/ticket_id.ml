type ticket_id =
  | Ticket_id of { ticketer : Deku_tezos.Contract_hash.t; data : bytes }
[@@deriving show, ord]

and t = ticket_id [@@deriving eq, ord]

let ticket_id_of_yojson json =
  match json with
  | `List
      [
        `String "Ticket_id";
        `Assoc [ ("ticketer", json); ("data", `String data) ];
      ] ->
      let ticketer = Deku_tezos.Contract_hash.t_of_yojson json in
      let data = Hex.to_string (`Hex data) |> Bytes.of_string in
      Ticket_id { ticketer : Deku_tezos.Contract_hash.t; data : bytes }
  | _ -> failwith "Wrong ticket_id format"

let yojson_of_ticket_id t =
  let (Ticket_id { ticketer; data }) = t in
  let ticketer = Deku_tezos.Contract_hash.yojson_of_t ticketer in
  let data = Bytes.to_string data |> Hex.of_string |> Hex.show in
  `List
    [
      `String "Ticket_id";
      `Assoc [ ("ticketer", ticketer); ("data", `String data) ];
    ]

let t_of_yojson = ticket_id_of_yojson
let yojson_of_t = yojson_of_ticket_id
let make ticketer data = Ticket_id { ticketer; data }

let from_tezos_ticket tezos_ticket =
  let Deku_tezos.Ticket_id.{ ticketer; data } = tezos_ticket in
  match ticketer with
  | Deku_tezos.Address.Implicit _address -> Error `Ticket_from_implicit
  | Deku_tezos.Address.Originated { contract; _ } ->
      Ok (Ticket_id { ticketer = contract; data })

let to_tezos_ticket ticket =
  let open Deku_tezos in
  let (Ticket_id { ticketer; data }) = ticket in
  let ticketer =
    Address.Originated { contract = ticketer; entrypoint = None }
  in
  Ticket_id.{ ticketer; data }

let of_string string =
  (* TODO: support Deku tickets *)
  let tezos_ticket =
    Deku_tezos.Ticket_id.of_string string
    |> Option.to_result ~none:`Cannot_parse
  in
  Result.bind tezos_ticket from_tezos_ticket

let to_string t =
  let open Tezos_micheline in
  match t with
  | Ticket_id { ticketer; data } ->
      let loc =
        let open Micheline_printer in
        { comment = None }
      in
      let address =
        Deku_tezos.Address.(
          Originated { contract = ticketer; entrypoint = None })
      in
      let micheline =
        Micheline.Prim
          ( loc,
            "Pair",
            [
              String (loc, Deku_tezos.Address.to_string address);
              Bytes (loc, data);
            ],
            [] )
      in
      Format.asprintf "%a" Micheline_printer.print_expr micheline

(* TODO: add Deku tickets back from v0 *)
