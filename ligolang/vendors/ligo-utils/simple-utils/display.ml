type json = Yojson.Safe.t

type 'a display_format =
  | Human_readable : string display_format
  | Dev : string display_format
  | Json : json display_format

type ex_display_format = Ex_display_format : 'a display_format -> ex_display_format

let human_readable = Ex_display_format Human_readable
let dev = Ex_display_format Dev
let json = Ex_display_format Json

type 'a pp = display_format:(string display_format) -> Format.formatter -> 'a -> unit
type 'a format = {
    pp : 'a pp ;
    to_json : 'a -> json ;
}

type 'a with_format = {
    value : 'a ;
    format : 'a format ;
}

type displayable = Displayable : 'a with_format -> displayable

let convert : type output . display_format:(output display_format) -> displayable -> output =
  fun ~display_format (Displayable { value ; format }) ->
  match display_format with
  | Json -> format.to_json value
  | Dev -> Format.asprintf "%a" (format.pp ~display_format) value
  | Human_readable -> Format.asprintf "%a" (format.pp ~display_format) value


let to_json : displayable -> json = convert ~display_format:Json

let bind_format :
  'value format -> 'error format -> ('value,'error) result format =
  fun value_format error_format ->
    let pp ~display_format f a = match a with
    | Error (e) -> error_format.pp ~display_format f e
    | Ok (v) -> value_format.pp ~display_format f v in
    let to_json a = match a with
      | Error (e) -> error_format.to_json e
      | Ok (v) -> value_format.to_json v in
    { pp ; to_json }
