open Simple_utils.Display
open Cst.Pascaligo

let stage = "self_cst_pascaligo"

type self_cst_pascaligo_error = [
  `Reserved_name of variable
| `Duplicate_variant of variable
| `Non_linear_pattern of variable
| `Non_linear_type_decl of type_var
| `Duplicate_field_name of variable
| `Duplicate_parameter of variable
]

let reserved_name var = `Reserved_name var
let duplicate_variant var = `Duplicate_variant var
let non_linear_pattern var = `Non_linear_pattern var
let duplicate_field_name var = `Duplicate_field_name var
let duplicate_parameter (var:string reg) = `Duplicate_parameter var
let non_linear_type_decl var = `Non_linear_type_decl var


let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_cst_pascaligo_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
      `Reserved_name var ->
      Format.fprintf f
        "@[<hv>%a@.Reserved name %S.@.Hint: Change the name.@]"
        Snippet.pp_lift var.region
        var.value
    | `Duplicate_variant var ->
      Format.fprintf f
        "Duplicate constructor %S in this sum type declaration.\n\
        Hint: Change the constructor.\n"
        var.value
    | `Non_linear_pattern var ->
      Format.fprintf f
        "@[<hv>%a@.Repeated variable %S in this pattern.@.Hint: Change the name.@]"
        Snippet.pp_lift var.region
        var.value
    | `Non_linear_type_decl var ->
      Format.fprintf f
        "@[<hv>%a@.Repeated type variable %S in type declaration.@.Hint: Change the name.@]"
        Snippet.pp_lift var.region
        var.value
    | `Duplicate_field_name var ->
      Format.fprintf f
        "Duplicate field name %S in this record declaration.\n\
        Hint: Change the name.\n"
        var.value
    | `Duplicate_parameter var ->
      Format.fprintf f
        "Duplicate parameter %S.\nHint: Change the name.\n"
        var.value
  )

let error_jsonformat : self_cst_pascaligo_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
    `Reserved_name var ->
    let message = `String "Reserved name" in
    let content = `Assoc [
      ("message", message );
      ("var", `String var.value);] in
    json_error ~stage ~content
  | `Duplicate_variant var ->
    let message = `String "Duplicate constructor in this sum type declaration."  in
    let content = `Assoc [
      ("message", message );
      ("var", `String var.value);] in
    json_error ~stage ~content
  | `Non_linear_pattern var ->
    let message = `String "Repeated variable in this pattern." in
    let content = `Assoc [
      ("message", message );
      ("var", `String var.value);] in
    json_error ~stage ~content
  | `Non_linear_type_decl var ->
    let message = `String "Repeated type variable in type declaration." in
    let content = `Assoc [
      ("message", message );
      ("var", `String var.value);] in
    json_error ~stage ~content
  | `Duplicate_field_name var ->
    let message = `String "Duplicate field name in this record declaration." in
    let content = `Assoc [
      ("message", message );
      ("var", `String var.value);] in
    json_error ~stage ~content
  | `Duplicate_parameter var ->
    let message = `String "Duplicate parameter \nHint: Change the name." in
    let content = `Assoc [
      ("message", message );
      ("var", `String var.value);] in
    json_error ~stage ~content
