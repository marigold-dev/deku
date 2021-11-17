open Simple_utils.Display
open Cst.Cameligo

let stage = "self_cst_cameligo"

type self_cst_cameligo_error = [
  `Reserved_name of variable
| `Duplicate_variant of variable
| `Non_linear_pattern of variable
| `Non_linear_type_decl of type_var reg
| `Duplicate_field_name of variable
]

let reserved_name var : self_cst_cameligo_error = `Reserved_name var
let duplicate_variant var : self_cst_cameligo_error = `Duplicate_variant var
let non_linear_pattern var : self_cst_cameligo_error = `Non_linear_pattern var
let duplicate_field_name var : self_cst_cameligo_error = `Duplicate_field_name var
let non_linear_type_decl var : self_cst_cameligo_error = `Non_linear_type_decl var

let error_ppformat :
  display_format:string display_format ->
  Format.formatter ->
  self_cst_cameligo_error ->
  unit =
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
        var.value.name.value
    | `Duplicate_field_name var ->
      Format.fprintf f
        "Duplicate field name %S in this record declaration.\n\
        Hint: Change the name.\n"
        var.value
  )

let mk_error (var: string Region.reg) (msg: string) =
  let loc = Location.lift @@ var.region in
  let content =
    `Assoc [("message",  `String msg);
            ("variable", `String var.value);
            ("location", Location.to_yojson loc)]
  in `Assoc [("status",  `String "error");
             ("stage",   `String stage);
             ("content",  content )]

let error_jsonformat : self_cst_cameligo_error -> Yojson.Safe.t =
  function
    `Reserved_name var ->
       mk_error var "Reserved name"
  | `Duplicate_variant var ->
       mk_error var
                "Duplicate constructor in this sum type declaration."
  | `Non_linear_pattern var ->
       mk_error var "Repeated variable in this pattern."
  | `Duplicate_field_name var ->
       mk_error var
                "Duplicate field name in this record declaration."
  | `Non_linear_type_decl var ->
    mk_error var.value.name
      "Repeated type variable in type declaration"
