open Display

let module_ppformat ~display_format f p =
  match display_format with
  | Human_readable | Dev -> PP.module_ f p

let module_jsonformat p : json =
  To_yojson.module_ p

let module_format : 'a format = {
  pp = module_ppformat;
  to_json = module_jsonformat;
}

let expression_ppformat ~display_format f (p,_) =
  match display_format with
  | Human_readable | Dev -> PP.expression f p

let expression_jsonformat (p,_) : json =
  let core' = Format.asprintf "%a" PP.expression p in
  `Assoc [("Core(temp)" , `String core')]

let expression_format : 'a format = {
  pp = expression_ppformat;
  to_json = expression_jsonformat;
}
