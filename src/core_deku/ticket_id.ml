open Bin_prot.Std

type ticketer =
  | Deku  of Address.t
  | Tezos of Tezos.Address.t
[@@deriving ord, eq, yojson, bin_io]

type t = {
  ticketer : ticketer;
  data : bytes;
}
[@@deriving eq, ord, yojson, bin_io]

let to_tezos = function
  | { ticketer = Deku _; data = _ } -> Error `Not_supported
  | { ticketer = Tezos ticketer; data } -> Ok Tezos.Ticket_id.{ ticketer; data }

let of_tezos = function
  | Tezos.Ticket_id.{ ticketer; data } -> Ok { ticketer = Tezos ticketer; data }

let is_tezos = function
  | { ticketer = Deku _; data = _ } -> false
  | { ticketer = Tezos _; data = _ } -> true

let mint_ticket ~contract_address ~data =
  { ticketer = Deku contract_address; data }

open Tezos_micheline
open Helpers

let parse_micheline string =
  let tokens, errors = Micheline_parser.tokenize string in
  match errors with
  | [] -> (
    let micheline, errors = Micheline_parser.parse_expression tokens in
    match errors with
    | [] -> Some micheline
    | _ -> None)
  | _ -> None

let ticketer_to_string t =
  match t.ticketer with
  | Deku t -> Address.to_string t
  | Tezos t -> Tezos.Address.to_string t

let to_string t =
  let loc =
    let open Micheline_printer in
    { comment = None } in
  let micheline =
    Micheline.Prim
      ( loc,
        "Pair",
        [String (loc, ticketer_to_string t); Bytes (loc, t.data)],
        [] ) in
  Format.asprintf "%a" Micheline_printer.print_expr micheline

let of_string string =
  let%some micheline = parse_micheline string in
  let%some ticketer, data =
    match micheline with
    | Prim (_, "Pair", [String (_, ticketer); Bytes (_, data)], []) ->
      Some (ticketer, data)
    | _ -> None in
  let%some ticketer =
    if Core.String.is_substring_at ~pos:0 ~substring:"DK1" ticketer then
      Address.of_string ticketer |> Option.map (fun x -> Deku x)
    else
      Tezos.Address.of_string ticketer |> Option.map (fun x -> Tezos x) in
  Some { ticketer; data }

let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "ticket_id" to_string of_string
