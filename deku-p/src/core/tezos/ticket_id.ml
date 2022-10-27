open Tezos_micheline
open Deku_stdlib

type t = { ticketer : Address.t; data : bytes } [@@deriving eq, ord, yojson]

let parse_micheline string =
  let tokens, errors = Micheline_parser.tokenize string in
  match errors with
  | [] -> (
      let micheline, errors = Micheline_parser.parse_expression tokens in
      match errors with [] -> Some micheline | _ -> None)
  | _ -> None

let to_string t =
  let loc =
    let open Micheline_printer in
    { comment = None }
  in
  let micheline =
    Micheline.Prim
      ( loc,
        "Pair",
        [ String (loc, Address.to_string t.ticketer); Bytes (loc, t.data) ],
        [] )
  in
  Format.asprintf "%a" Micheline_printer.print_expr micheline

let of_string string =
  let%some micheline = parse_micheline string in
  let%some ticketer, data =
    match micheline with
    | Prim (_, "Pair", [ String (_, ticketer); Bytes (_, data) ], []) ->
        Some (ticketer, data)
    | _ -> None
  in
  let%some ticketer = Address.of_string ticketer in
  Some { ticketer; data }

let encoding =
  let open Data_encoding in
  conv
    (fun { ticketer; data } -> (data, ticketer))
    (fun (data, ticketer) -> { ticketer; data })
    (dynamic_size (tup2 bytes Address.encoding))
