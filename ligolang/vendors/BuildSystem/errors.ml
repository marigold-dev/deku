type t =
[
 | `Build_dependency_cycle of string
 | `Build_corner_case of string * string (* TO REMOVE *)
]
let build_dependency_cycle (s:string) = `Build_dependency_cycle s
let build_corner_case (loc:string) (msg:string)  = `Build_corner_case (loc,msg)

open Simple_utils.Display

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> t -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Build_dependency_cycle trace ->
      Format.fprintf f "@[<hv>Dependency cycle detected :@, %s@]" trace
    | `Build_corner_case (loc,msg) ->
      Format.fprintf f "@[<hv>Building corner case at %s : %s@]" loc msg
  )

let rec error_jsonformat : t -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Build_dependency_cycle trace ->
    let content = `Assoc [
      ("message", `String "dependency cycle detected") ;
      ("cycle",    `String trace) ; ] in
    json_error ~stage:"build system" ~content
  | `Build_corner_case (loc,msg) ->
    let content = `Assoc [
      ("message", `String msg) ;
      ("loc", `String loc) ]
    in
    json_error ~stage:"build system" ~content

let error_format : _ format = {
  pp = error_ppformat;
  to_json = error_jsonformat;
}
