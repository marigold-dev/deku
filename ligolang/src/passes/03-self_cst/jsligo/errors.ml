open Simple_utils.Display
open Cst.Jsligo

let stage = "self_cst_jsligo"

type self_cst_jsligo_error = [
  `Reserved_name of variable
| `Duplicate_variant of variable
| `Non_linear_pattern of variable
| `Non_linear_type_decl of type_var
| `Duplicate_field_name of variable
| `Not_supported_variant of type_expr
]

let reserved_name var : self_cst_jsligo_error = `Reserved_name var
let duplicate_variant var : self_cst_jsligo_error = `Duplicate_variant var
let non_linear_pattern var : self_cst_jsligo_error = `Non_linear_pattern var
let duplicate_field_name var : self_cst_jsligo_error = `Duplicate_field_name var
let not_supported_variant t : self_cst_jsligo_error = `Not_supported_variant t
let non_linear_type_decl var : self_cst_jsligo_error = `Non_linear_type_decl var

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_cst_jsligo_error -> unit =
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
    | `Not_supported_variant _t ->
      Format.fprintf f
        "Not supported variant type. "
  )

let error_jsonformat : self_cst_jsligo_error -> Yojson.Safe.t = fun a ->
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
    let message = `String "Repeated type variable in type declaration" in
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
  | `Not_supported_variant _t ->
    let message = `String "Not supported variant type. " in
    let content = `Assoc [
      ("message", message );
      (* ("var", `String var.value);]  *)
    ]
    in
    json_error ~stage ~content