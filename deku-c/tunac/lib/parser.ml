open Helpers
open Tezos_micheline
module MPrim = Michelson_primitives

let to_parsing_error error = Result.wrap ~f:(fun x -> `Parsing_error x) error

let to_prim_parsing_error error =
  Result.wrap ~f:(fun x -> `Prim_parsing_error x) error

let parse_expr expr =
  let open Result.Let_syntax in
  let* tokenized =
    Micheline_parser.tokenize expr
    |> Micheline_parser.no_parsing_error |> to_parsing_error
  in
  let* parsed =
    Micheline_parser.parse_expression tokenized
    |> Micheline_parser.no_parsing_error |> to_parsing_error
  in
  let* x =
    parsed |> Micheline.strip_locations |> MPrim.prims_of_strings
    |> to_prim_parsing_error
  in
  Ok x
