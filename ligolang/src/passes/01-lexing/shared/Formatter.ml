open Simple_utils.Display

let ppx_ppformat ~display_format f buf =
  match display_format with
  | Human_readable | Dev -> Format.fprintf f "%s" (Buffer.contents buf)

let ppx_jsonformat buf : json =
  let s = Format.asprintf "%s" (Buffer.contents buf) in
  `String s

let ppx_format : 'a format = {
  pp = ppx_ppformat;
  to_json = ppx_jsonformat;
}
