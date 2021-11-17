open Display
open Simple_utils.Runned_result

let failwith_to_string (f:failwith) : string =
  let str = match f with
  | Failwith_int i -> string_of_int i
  | Failwith_string s -> Format.asprintf "\"%s\"" (String.escaped s)
  | Failwith_bytes b ->
    Format.asprintf "0X%a" Hex.pp (Hex.of_bytes b) in
  Format.asprintf "failwith(%s)" str

let expression_ppformat ~display_format f runned_result =
  match display_format with
  | Display.Human_readable | Dev -> (
    match runned_result with
    | Fail fail_res ->
      let failstring = failwith_to_string fail_res in
      Format.pp_print_string f failstring
    | Success typed ->
      Ast_core.PP.expression f typed      
  )

let expression_jsonformat runned_result : Display.json =
  match runned_result with
  | Fail fail_res ->
    let failstring = failwith_to_string fail_res in
    `Assoc [("value", `Null) ; ("failure", `String failstring)]
  | Success typed ->
    `Assoc [("value", Ast_core.Yojson.expression typed) ; ("failure", `Null)]

let expression_format : 'a Display.format = {
  pp = expression_ppformat ;
  to_json = expression_jsonformat ;
}
