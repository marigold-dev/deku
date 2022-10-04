type ticketer =
  | Tezos of Deku_tezos.Contract_hash.t
  | Deku of Contract_address.t
[@@deriving eq, ord, show]

type ticket_id = Ticket_id of { ticketer : ticketer; data : bytes }
[@@deriving show, ord]

and t = ticket_id [@@deriving eq, ord]

let ticket_id_of_yojson json =
  match json with
  | `List
      [
        `String "Ticket_id";
        `Assoc [ ("ticketer", json); ("data", `String data) ];
      ] ->
      let ticketer =
        try Tezos (Deku_tezos.Contract_hash.t_of_yojson json)
        with _ -> Deku (Contract_address.t_of_yojson json)
      in
      let data = Hex.to_string (`Hex data) |> Bytes.of_string in
      Ticket_id { ticketer; data : bytes }
  | _ -> failwith "Wrong ticket_id format"

let yojson_of_ticket_id t =
  let (Ticket_id { ticketer; data }) = t in
  let ticketer =
    match ticketer with
    | Tezos ticketer -> Deku_tezos.Contract_hash.yojson_of_t ticketer
    | Deku ticketer -> Contract_address.yojson_of_t ticketer
  in
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
      Ok (Ticket_id { ticketer = Tezos contract; data })

let to_tezos_ticket ticket =
  let open Deku_tezos in
  let (Ticket_id { ticketer; data }) = ticket in
  match ticketer with
  | Deku _ -> None
  | Tezos ticketer ->
      let ticketer =
        Address.Originated { contract = ticketer; entrypoint = None }
      in
      Some Ticket_id.{ ticketer; data }

let parse_micheline string =
  let open Tezos_micheline in
  let tokens, errors = Micheline_parser.tokenize string in
  match errors with
  | [] -> (
      let micheline, errors = Micheline_parser.parse_expression tokens in
      match errors with [] -> Some micheline | _ -> None)
  | _ -> None

let of_string string =
  let open Deku_stdlib in
  let%ok micheline =
    parse_micheline string |> Option.to_result ~none:`Cannot_parse
  in
  let%ok ticketer, data =
    (match micheline with
    | Prim (_, "Pair", [ String (_, ticketer); Bytes (_, data) ], []) ->
        Some (ticketer, data)
    | _ -> None)
    |> Option.to_result ~none:`Cannot_parse
  in
  let%ok ticketer =
    Deku_tezos.Contract_hash.of_string ticketer
    |> Option.map (fun x -> Some (Tezos x))
    |> Option.value
         ~default:
           (Contract_address.of_b58 ticketer |> Option.map (fun x -> Deku x))
    |> Option.to_result ~none:`Cannot_parse
  in
  Some (Ticket_id { ticketer; data }) |> Option.to_result ~none:`Cannot_parse

let to_string t =
  let open Tezos_micheline in
  match t with
  | Ticket_id { ticketer = Tezos ticketer; data } ->
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
  | Ticket_id { ticketer = Deku ticketer; data } ->
      let loc =
        let open Micheline_printer in
        { comment = None }
      in
      let address = ticketer in
      let micheline =
        Micheline.Prim
          ( loc,
            "Pair",
            [ String (loc, Contract_address.to_b58 address); Bytes (loc, data) ],
            [] )
      in
      Format.asprintf "%a" Micheline_printer.print_expr micheline

(* TODO: add Deku tickets back from v0 *)
