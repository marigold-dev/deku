include Z

exception Not_an_integer
exception Not_a_string

let z_of_yojson json =
  match json with
  | `String string -> (
      try Z.of_string string with Invalid_argument _ -> raise Not_an_integer)
  | _ -> raise Not_a_string

let yojson_of_z z = `String (Z.to_string z)
