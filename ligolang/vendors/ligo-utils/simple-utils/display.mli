type json = Yojson.Safe.t

type 'a display_format =
  | Human_readable : string display_format
  | Dev : string display_format
  | Json : json display_format

type ex_display_format = Ex_display_format : 'a display_format -> ex_display_format

val human_readable : ex_display_format
val dev : ex_display_format
val json : ex_display_format

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

val convert : display_format:'output display_format -> displayable -> 'output

val to_json : displayable -> json

val bind_format : 'value format -> 'error format -> ('value,'error) result format
