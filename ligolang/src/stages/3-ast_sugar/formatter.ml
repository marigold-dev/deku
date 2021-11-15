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
