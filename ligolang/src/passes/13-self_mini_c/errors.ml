open Simple_utils.Display

let stage = "self_mini_c"

type self_mini_c_error = [
  | `Self_mini_c_bad_self_address of Mini_c.constant'
  | `Self_mini_c_not_a_function
  | `Self_mini_c_could_not_aggregate_entry
] [@@deriving poly_constructor { prefix = "self_mini_c_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_mini_c_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_mini_c_bad_self_address _cst ->
      let s = Format.asprintf "\"Tezos.self\" must be used directly and cannot be used via another function." in
      Format.pp_print_string f s ;
    | `Self_mini_c_not_a_function -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operations list * storage\"."
    | `Self_mini_c_could_not_aggregate_entry -> Format.fprintf f "Invalid type for entrypoint.@.An entrypoint must of type \"parameter * storage -> operations list * storage\"."
  )

let error_jsonformat : self_mini_c_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_mini_c_bad_self_address cst ->
    let msg = Format.asprintf "%a is only allowed at top-level" Stage_common.PP.constant' cst in
    let content = `Assoc [
      ("message", `String msg); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_not_a_function -> 
    let content = `Assoc [
      ("message", `String "getting function has failed"); ]
    in
    json_error ~stage ~content
  | `Self_mini_c_could_not_aggregate_entry ->
    let content = `Assoc [
      ("message", `String "could not aggregate"); ]
    in
    json_error ~stage ~content
