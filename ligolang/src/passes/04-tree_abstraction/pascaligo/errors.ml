open Simple_utils.Display

module Raw = Cst.Pascaligo

let stage = "abstracter"

type abs_error = [
  | `Concrete_pascaligo_unknown_constant of string * Location.t
  | `Concrete_pascaligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_pascaligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_pascaligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_pascaligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_pascaligo_recursive_fun of Location.t
  | `Concrete_pascaligo_block_attribute of Raw.block Region.reg
  | `Concrete_pascaligo_unsupported_top_level_destructuring of Region.t
  ]

let unsupported_top_level_destructuring loc =
  `Concrete_pascaligo_unsupported_top_level_destructuring loc

let unknown_predefined_type name =
  `Concrete_pascaligo_unknown_predefined_type name

let unknown_constant s loc =
  `Concrete_pascaligo_unknown_constant (s,loc)

let untyped_recursive_fun loc =
  `Concrete_pascaligo_recursive_fun loc

let unsupported_pattern_type pl =
  `Concrete_pascaligo_unsupported_pattern_type pl

let unsupported_string_singleton te =
  `Concrete_pascaligo_unsupported_string_singleton te

let michelson_type_wrong texpr name =
  `Concrete_pascaligo_michelson_type_wrong (texpr,name)

let michelson_type_wrong_arity loc name =
  `Concrete_pascaligo_michelson_type_wrong_arity (loc,name)

let block_start_with_attribute block =
  `Concrete_pascaligo_block_attribute block

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_pascaligo_unknown_constant (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp loc s
    | `Concrete_pascaligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid case pattern.\
        @.Can't match on values. @]"
        Snippet.pp_lift @@ Raw.pattern_to_region pl
    | `Concrete_pascaligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Snippet.pp_lift (Raw.type_expr_to_region texpr)
          name
    | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_pascaligo_recursive_fun loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp loc
    | `Concrete_pascaligo_block_attribute block ->
      Format.fprintf f
        "@[<hv>%a@.Invalid attribute declaration.@.Attributes have to follow the declaration it is attached to. @]"
        Snippet.pp_lift @@ block.region
    | `Concrete_pascaligo_unsupported_top_level_destructuring loc ->
      Format.fprintf f
        "@[<hv>%a@.Unsupported destructuring at top-level. @]"
        Snippet.pp_lift @@ loc
  )


let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_pascaligo_unsupported_top_level_destructuring loc ->
    let message = `String "Unsupported destructuring at top-level" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson (Snippet.lift loc));] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unknown_constant (s,loc) ->
    let message = `String ("Unknow constant: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_recursive_fun loc ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_pattern_type pl ->
    let loc = Format.asprintf "%a"
      Location.pp_lift @@ Raw.pattern_to_region pl in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", `String loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument %s of %s must be a string singleton"
        (Cst_pascaligo.Printer.type_expr_to_string ~offsets:true ~mode:`Point texpr) name in
    let loc = Format.asprintf "%a" Location.pp_lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_block_attribute block ->
    let message = Format.asprintf "Attributes have to follow the declaration it is attached" in
    let loc = Format.asprintf "%a" Location.pp_lift block.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", `String loc); ] in
    json_error ~stage ~content
