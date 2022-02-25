open Result_ext.Let_syntax
let with_yojson_string name to_string of_string =
  let to_yojson t = `String (to_string t) in
  let of_yojson json =
    let%ok string = [%of_yojson: string] json in
    of_string string |> Option.to_result ~none:("invalid " ^ name) in
  (to_yojson, of_yojson)

let rec of_data_encoding_json (json : Data_encoding.json) : Yojson.Safe.t =
  match json with
  | `O properties ->
    let properties =
      List.map
        (fun (key, value) -> (key, of_data_encoding_json value))
        properties in
    `Assoc properties
  | `A elements ->
    let elements = List.map of_data_encoding_json elements in
    `List elements
  | `Bool bool -> `Bool bool
  | `Float float -> `Float float
  | `Null -> `Null
  | `String string -> `String string

let rec to_data_encoding_json (json : Yojson.Basic.t) : Data_encoding.Json.t =
  match json with
  | `Assoc properties ->
    let properties =
      List.map
        (fun (key, value) -> (key, to_data_encoding_json value))
        properties in
    `O properties
  | `List elements ->
    let elements = List.map to_data_encoding_json elements in
    `A elements
  | `Bool bool -> `Bool bool
  | `Float float -> `Float float
  | `Null -> `Null
  | `String string -> `String string
  | `Int int -> `Float (float_of_int int)
let to_data_encoding_json json =
  json |> Yojson.Safe.to_basic |> to_data_encoding_json

let with_data_encoding encoder =
  let to_yojson t =
    let json = Data_encoding.Json.construct encoder t in
    of_data_encoding_json json in
  let of_yojson json =
    let json = to_data_encoding_json json in
    try Ok (Data_encoding.Json.destruct encoder json) with
    | exn ->
      let print_unknown _fmt exn = raise exn in
      let err =
        Format.asprintf "%a" (Data_encoding.Json.print_error ~print_unknown) exn
      in
      Error err in
  (to_yojson, of_yojson)
