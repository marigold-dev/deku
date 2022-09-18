include Uri

let t_of_yojson json =
  let string = Yojson.Safe.Util.to_string json in
  Uri.of_string string

let yojson_of_t uri = `String (Uri.to_string uri)

let cmdliner_converter =
  (* TODO: add more validation to URI parsing than what the uri library provides *)
  let of_string s = `Ok (Uri.of_string s) in
  let to_string = Uri.pp in
  (of_string, to_string)
