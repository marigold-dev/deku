open Simple_utils.Display
open Simple_utils

module Trace = Simple_utils.Trace

let warn_str (display_format:ex_display_format) (a: 'a list) : string =
  let (Ex_display_format t) = display_format in
  match t with
  | Human_readable | Dev as t ->
     Format.asprintf "%a\n" (Simple_utils.PP_helpers.list_sep (Main_errors.Formatter.error_format.pp ~display_format:t) (Simple_utils.PP_helpers.tag "")) a
  | Json -> let json = List.map ~f:Main_errors.Formatter.error_format.to_json a in
            let s = Yojson.Safe.pretty_to_string @@ `List json in
            Format.asprintf "%s\n" s

let toplevel : ?werror:bool -> display_format:ex_display_format -> displayable -> (unit -> Main_warnings.all list) -> ('value, _) result -> _ =
  fun ?(werror=false) ~display_format disp warns value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
    in
    let warns = warns () in
    let warns = List.map warns ~f:(fun value ->
      match t with
        ( Human_readable | Dev) as s -> convert ~display_format:s (Displayable {value;format=Main_warnings.format})
        | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t (Displayable {value;format=Main_warnings.format})) in        
    let warns_str = String.concat "\n" warns in
    if not (List.is_empty warns) && werror then
        Error (warns_str,warns_str)
    else
    match value with
    | Ok _ -> Ok (as_str,warns_str)
    | Error _ -> Error (as_str,warns_str)

let format_result : ?werror:bool -> display_format:ex_display_format -> 'value format -> (unit -> Main_warnings.all list) -> (raise:Main_errors.all Trace.raise -> 'value) -> _ =
  fun ?(werror=false) ~display_format value_format warns value ->
    let format = bind_format value_format Main_errors.Formatter.error_format in
    let value = Trace.to_stdlib_result value in
    toplevel ~werror ~display_format (Displayable {value ; format}) warns value
