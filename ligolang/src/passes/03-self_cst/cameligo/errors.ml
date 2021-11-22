open Simple_utils.Display
open Cst.Cameligo

let stage = "self_cst_cameligo"

type self_cst_cameligo_error = [
  `Self_cst_cameligo_reserved_name of variable
| `Self_cst_cameligo_duplicate_variant of variable
| `Self_cst_cameligo_non_linear_pattern of variable
| `Self_cst_cameligo_non_linear_type_decl of type_var reg
| `Self_cst_cameligo_duplicate_field_name of variable
] [@@deriving poly_constructor { prefix = "self_cst_cameligo_" }]

let error_ppformat :
  display_format:string display_format ->
  Format.formatter ->
  self_cst_cameligo_error ->
  unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
      `Self_cst_cameligo_reserved_name var ->
      Format.fprintf f
        "@[<hv>%a@.Reserved name %S.@.Hint: Change the name.@]"
        Snippet.pp_lift var.region
        var.value
    | `Self_cst_cameligo_duplicate_variant var ->
      Format.fprintf f
        "Duplicate constructor %S in this sum type declaration.\n\
        Hint: Change the constructor.\n"
        var.value
    | `Self_cst_cameligo_non_linear_pattern var ->
      Format.fprintf f
        "@[<hv>%a@.Repeated variable %S in this pattern.@.Hint: Change the name.@]"
        Snippet.pp_lift var.region
        var.value
    | `Self_cst_cameligo_non_linear_type_decl var ->
      Format.fprintf f
        "@[<hv>%a@.Repeated type variable %S in type declaration.@.Hint: Change the name.@]"
        Snippet.pp_lift var.region
        var.value.name.value
    | `Self_cst_cameligo_duplicate_field_name var ->
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
    `Self_cst_cameligo_reserved_name var ->
       mk_error var "Reserved name"
  | `Self_cst_cameligo_duplicate_variant var ->
       mk_error var
                "Duplicate constructor in this sum type declaration."
  | `Self_cst_cameligo_non_linear_pattern var ->
       mk_error var "Repeated variable in this pattern."
  | `Self_cst_cameligo_duplicate_field_name var ->
       mk_error var
                "Duplicate field name in this record declaration."
  | `Self_cst_cameligo_non_linear_type_decl var ->
    mk_error var.value.name
      "Repeated type variable in type declaration"
