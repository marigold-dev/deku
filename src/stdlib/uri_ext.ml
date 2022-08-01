include Uri

let t_of_yojson json =
  let string = Yojson.Safe.Util.to_string json in
  Uri.of_string string

let yojson_of_t uri = `String (Uri.to_string uri)
