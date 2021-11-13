open Display

let scope_ppformat ~display_format f (d,s) =
  match display_format with
  | Human_readable ->
    Format.fprintf f "there is to human-readable pretty printer for you, use --format=json"
  | Dev -> Format.fprintf f "@[<v>%a@ %a@]" PP.scopes s PP.definitions d

let scope_jsonformat defscopes : json = PP.to_json defscopes

let scope_format : 'a format = {
  pp = scope_ppformat;
  to_json = scope_jsonformat;
}
