open Simple_utils.Display

module Raw = Cst.Cameligo
module Parsing = Parsing.Cameligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_cameligo_recursive_fun of Region.t
  | `Concrete_cameligo_unknown_constant of string * Location.t
  | `Concrete_cameligo_unsupported_pattern_type of Raw.pattern list
  | `Concrete_cameligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_cameligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_cameligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_cameligo_recursion_on_non_function of Location.t
  | `Concrete_cameligo_missing_funarg_annotation of Raw.variable
  | `Concrete_cameligo_funarg_tuple_type_mismatch of Region.t * Raw.pattern * Raw.type_expr
  | `Concrete_cameligo_type_params_not_annotated of Region.t
  ]

let untyped_recursive_fun reg : abs_error =
  `Concrete_cameligo_recursive_fun reg
let unknown_constant s loc : abs_error =
  `Concrete_cameligo_unknown_constant (s,loc)

let unsupported_pattern_type pl : abs_error =
  `Concrete_cameligo_unsupported_pattern_type pl

let unsupported_string_singleton te : abs_error =
  `Concrete_cameligo_unsupported_string_singleton te

let recursion_on_non_function reg : abs_error =
  `Concrete_cameligo_recursion_on_non_function reg

let michelson_type_wrong texpr name : abs_error =
  `Concrete_cameligo_michelson_type_wrong (texpr,name)

let michelson_type_wrong_arity loc name : abs_error =
  `Concrete_cameligo_michelson_type_wrong_arity (loc,name)

let missing_funarg_annotation v : abs_error =
  `Concrete_cameligo_missing_funarg_annotation v

let funarg_tuple_type_mismatch r p t : abs_error =
  `Concrete_cameligo_funarg_tuple_type_mismatch (r, p, t)

let type_params_not_annotated r : abs_error =
  `Concrete_cameligo_type_params_not_annotated r

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_cameligo_recursive_fun reg ->
      Format.fprintf f
      "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp_lift reg
    | `Concrete_cameligo_unknown_constant (s,reg) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp reg s
    | `Concrete_cameligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid pattern.@.Can't match on values. @]"
        Snippet.pp_lift (List.fold_left ~f:(fun a p -> Region.cover a (Raw.pattern_to_region p)) ~init:Region.ghost pl)
    | `Concrete_cameligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_cameligo_recursion_on_non_function reg ->
      Format.fprintf f "@[<hv>%a@.Invalid let declaration.@.Only functions can be recursive. @]"
        Snippet.pp reg
    | `Concrete_cameligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
      "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
         Snippet.pp_lift (Raw.type_expr_to_region texpr)
         name
    | `Concrete_cameligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_cameligo_missing_funarg_annotation v ->
      Format.fprintf f
        "@[<hv>%a@.Missing a type annotation for argument \"%s\". @]"
          Snippet.pp_lift v.region
          v.value
    | `Concrete_cameligo_funarg_tuple_type_mismatch (region, pattern, texpr) -> (
      let p = Parsing.pretty_print_pattern pattern |> Buffer.contents in
      let t = Parsing.pretty_print_type_expr texpr |> Buffer.contents in
      Format.fprintf
        f
        "@[<hv>%a@.The tuple \"%s\" does not match the type \"%s\". @]"
        Snippet.pp_lift region
        p
        t
    )
    | `Concrete_cameligo_type_params_not_annotated reg ->
      Format.fprintf f
        "@[<hv>%a@.Functions with type parameters need to be annotated. @]"
        Snippet.pp_lift reg
  )


let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_cameligo_recursive_fun reg ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp_lift reg in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unknown_constant (s,loc) ->
    let message = `String ("Unknow constant: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
 | `Concrete_cameligo_unsupported_pattern_type pl ->
    let loc = Format.asprintf "%a"
      Location.pp_lift (List.fold_left ~f:(fun a p -> Region.cover a (Raw.pattern_to_region p)) ~init:Region.ghost pl) in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_cameligo_recursion_on_non_function reg ->
    let message = Format.asprintf "Only functions can be recursive." in
    let loc = Format.asprintf "%a" Location.pp reg in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc) ] in
    json_error ~stage ~content
  | `Concrete_cameligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument %s of %s must be a string singleton"
        (Cst_cameligo.Printer.type_expr_to_string ~offsets:true ~mode:`Point texpr) name in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_missing_funarg_annotation v ->
    let message = Format.asprintf "Missing type annotation for argument \"%s\"" v.value in
    let loc = Format.asprintf "%a" Location.pp_lift v.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_cameligo_funarg_tuple_type_mismatch (r, _, _) ->
    let message = Format.asprintf "The tuple does not match the type." in
    let loc = Format.asprintf "%a" Location.pp_lift r in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc);
    ] in
    json_error ~stage ~content
  | `Concrete_cameligo_type_params_not_annotated r ->
    let message = Format.asprintf "Functions with type parameters need to be annotated." in
    let loc = Format.asprintf "%a" Location.pp_lift r in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc);
    ] in
    json_error ~stage ~content
