open Display

let module_ppformat_fully_typed ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.module_fully_typed f typed

let module_jsonformat_fully_typed p : json =
  To_yojson.module_fully_typed p

let module_ppformat_with_unification_vars ~display_format f typed =
  match display_format with
  | Human_readable | Dev -> PP.module_with_unification_vars f typed

let module_jsonformat_with_unification_vars p : json =
  To_yojson.module_with_unification_vars p

let module_format_fully_typed : 'a format = {
  pp = module_ppformat_fully_typed;
  to_json = module_jsonformat_fully_typed;
}

let module_format_with_unification_vars : 'a format = {
  pp = module_ppformat_with_unification_vars;
  to_json = module_jsonformat_with_unification_vars;
}
