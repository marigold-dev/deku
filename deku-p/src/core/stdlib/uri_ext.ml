include Uri

let cmdliner_converter =
  (* TODO: add more validation to URI parsing than what the uri library provides *)
  let of_string s = `Ok (Uri.of_string s) in
  let to_string = Uri.pp in
  (of_string, to_string)
