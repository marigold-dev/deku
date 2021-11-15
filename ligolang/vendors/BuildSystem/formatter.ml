open Simple_utils.Display

let graph_ppformat ~display_format f g =
  match display_format with
  | Human_readable | Dev -> PP.graph f g

let graph_jsonformat g : json =
  To_yojson.graph g

let graph_format : 'a format = {
  pp = graph_ppformat;
  to_json = graph_jsonformat;
}
