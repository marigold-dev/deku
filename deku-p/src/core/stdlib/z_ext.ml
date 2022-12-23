include Z

exception Not_a_string
exception Not_an_integer

let t_of_yojson json =
  match json with
  | `String string -> (
      try Z.of_string string with Invalid_argument _ -> raise Not_an_integer)
  | _ -> raise Not_a_string

let yojson_of_t t = `String (Z.to_string t)