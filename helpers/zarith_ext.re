include Z;
let to_yojson = z => `String(Z.to_string(z));
let of_yojson =
  fun
  | `String(string) =>
    try(Ok(Z.of_string(string))) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");
