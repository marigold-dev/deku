open Simple_utils.Display

let stage = "self_ast_typed"

type self_ast_typed_error = [
  | `Self_ast_typed_rec_call of Ast_typed.expression_variable * Location.t
  | `Self_ast_typed_bad_self_type of Ast_typed.type_expression * Ast_typed.type_expression * Location.t
  | `Self_ast_typed_format_entrypoint_ann of string * Location.t
  | `Self_ast_typed_entrypoint_ann_not_literal of Location.t
  | `Self_ast_typed_unmatched_entrypoint of Location.t
  | `Self_ast_typed_nested_big_map of Location.t
  | `Self_ast_typed_corner_case of string
  | `Self_ast_typed_contract_io of string * Ast_typed.expression
  | `Self_ast_typed_contract_list_ops of string * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_expected_same_entry of
    string * Ast_typed.type_expression * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_pair_in of Location.t * [`View | `Contract]
  | `Self_ast_typed_pair_out of Location.t
  | `Self_ast_typed_match_anomaly of Location.t
  | `Self_ast_typed_obj_ligo of Location.t
  | `Self_ast_typed_view_contract of Location.t * string * string * Ast_typed.type_expression * Ast_typed.type_expression
  | `Self_ast_typed_view_io of Location.t * Ast_typed.type_expression * [`In | `Out]
]

let pattern_matching_anomaly (loc:Location.t) : self_ast_typed_error = `Self_ast_typed_match_anomaly loc
let recursive_call_is_only_allowed_as_the_last_operation name loc =
  `Self_ast_typed_rec_call (name,loc)
let bad_self_type expected got loc =
  `Self_ast_typed_bad_self_type (expected,got,loc)
let bad_format_entrypoint_ann ep loc =
  `Self_ast_typed_format_entrypoint_ann (ep,loc)
let entrypoint_annotation_not_literal loc =
  `Self_ast_typed_entrypoint_ann_not_literal loc
let unmatched_entrypoint loc =
  `Self_ast_typed_unmatched_entrypoint loc
let nested_bigmap loc = `Self_ast_typed_nested_big_map loc
let corner_case s = `Self_ast_typed_corner_case s
let bad_contract_io entrypoint e = `Self_ast_typed_contract_io (entrypoint, e)
let expected_list_operation entrypoint got e =
  `Self_ast_typed_contract_list_ops (entrypoint, got, e)
let expected_same entrypoint t1 t2 e =
  `Self_ast_typed_expected_same_entry (entrypoint,t1,t2,e)
let expected_pair_in_contract loc = `Self_ast_typed_pair_in (loc , `Contract)
let expected_pair_in_view loc = `Self_ast_typed_pair_in (loc, `View)
let expected_pair_out loc = `Self_ast_typed_pair_out loc
let expected_obj_ligo loc = `Self_ast_typed_obj_ligo loc
let storage_view_contract loc main_name view_name ct vt = `Self_ast_typed_view_contract (loc,main_name,view_name,ct,vt)
let type_view_io_in loc got = `Self_ast_typed_view_io (loc,got,`In)
let type_view_io_out loc got = `Self_ast_typed_view_io (loc,got,`Out)

let error_ppformat : display_format:string display_format ->
  Format.formatter -> self_ast_typed_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_typed_view_contract (loc,main_name,view_name,ct,vt) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid view argument.@.View '%s' has storage type '%a' and contract '%s' has storage type '%a'.@]"
        Snippet.pp loc
        view_name
        Ast_typed.PP.type_expression vt
        main_name
        Ast_typed.PP.type_expression ct
    | `Self_ast_typed_view_io (loc,got,arg) ->
      let s = match arg with
        | `In -> "input"
        | `Out -> "output"
      in
      Format.fprintf f
        "@[<hv>%a@.Invalid view.@.Type '%a' is forbidden as %s argument.@]"
        Snippet.pp loc
        Ast_typed.PP.type_expression got
        s
    | `Self_ast_typed_match_anomaly loc ->
      Format.fprintf f
        "@[<hv>%a@.Pattern matching anomaly (redundant, or non exhaustive). @]"
        Snippet.pp loc
    | `Self_ast_typed_rec_call (_name,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Recursive call not in tail position. @.The value of a recursive call must be immediately returned by the defined function. @]"
        Snippet.pp loc
    | `Self_ast_typed_bad_self_type (expected,got,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type annotation.@.\"%a\" was given, but \"%a\" was expected.@.Note that \"Tezos.self\" refers to this contract, so the parameters should be the same. @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression got
        Ast_typed.PP.type_expression expected
    | `Self_ast_typed_format_entrypoint_ann (ep,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint \"%s\". One of the following patterns is expected:@.* \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when no entrypoint is used."
        Snippet.pp loc
        ep
    | `Self_ast_typed_entrypoint_ann_not_literal loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value must be a string literal. @]"
        Snippet.pp loc
    | `Self_ast_typed_unmatched_entrypoint loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value does not match a constructor of the contract parameter. @]"
        Snippet.pp loc
    | `Self_ast_typed_nested_big_map loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid big map nesting.@.A big map cannot be nested inside another big map. @]"
        Snippet.pp loc
    | `Self_ast_typed_corner_case desc ->
      Format.fprintf f
        "@[<hv>Internal error: %s @]"
        desc
    | `Self_ast_typed_contract_io (entrypoint, e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type for entrypoint \"%s\".@.An entrypoint must of type \"parameter * storage -> operations list * storage\". @]"
        Snippet.pp e.location
        entrypoint
    | `Self_ast_typed_contract_list_ops (entrypoint, got, e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type for entrypoint \"%s\".@.An entrypoint must of type \"parameter * storage -> operations list * storage\".@.\
        We expected a list of operations but we got %a@]"
        Snippet.pp e.location
        entrypoint
        Ast_typed.PP.type_expression got
    | `Self_ast_typed_expected_same_entry (entrypoint,t1,t2,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type for entrypoint \"%s\".@.The storage type \"%a\" of the function parameter must be the same as the storage type \"%a\" of the return value.@]"
        Snippet.pp e.location
        entrypoint
        Ast_typed.PP.type_expression t1
        Ast_typed.PP.type_expression t2
    | `Self_ast_typed_pair_in (loc,t) ->
      let ep = match t with `View -> "view" | `Contract -> "contract" in
      Format.fprintf f
        "@[<hv>%a@.Invalid %s.@.Expected a tuple as argument.@]"
        Snippet.pp loc
        ep
    | `Self_ast_typed_pair_out loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid entrypoint.@.Expected a tuple of operations and storage as return value.@]"
        Snippet.pp loc
    | `Self_ast_typed_obj_ligo loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid call to Test primitive.@]"
        Snippet.pp loc
  )

let error_jsonformat : self_ast_typed_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_typed_view_contract (loc,main_name,view_name,_,_) ->
    let message = `String "Invalid view argument" in
    let content = `Assoc [
      ("message", message);
      ("loc", Location.to_yojson loc);
      ("main_name", `String main_name);
      ("view_name", `String view_name);
      ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_view_io (loc,_,_) ->
    let message = `String "Invalid view argument" in
    let content = `Assoc [
      ("message", message);
      ("loc", Location.to_yojson loc);
      ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_match_anomaly loc ->
    let message = `String "pattern matching anomaly" in
    let content = `Assoc [
      ("message", message);
      ("loc", Location.to_yojson loc);
      ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_rec_call (name,loc) ->
    let message = `String "recursion must be achieved through tail-calls only" in
    let fn = `String (Format.asprintf "%a" Ast_typed.PP.expression_variable name) in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("function", fn);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_bad_self_type (expected,got,loc) ->
    let message = `String "bad self type" in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression expected) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression got) in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("expected", expected);
       ("actual", actual);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_format_entrypoint_ann (ep,loc) ->
    let message = `String "bad entrypoint format" in
    let entrypoint = `String ep in
    let hint = `String "we expect '%%bar' for entrypoint Bar and '%%default' when no entrypoint used" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("hint", hint);
       ("entrypoint", entrypoint);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_entrypoint_ann_not_literal loc ->
    let message = `String "entrypoint annotation must be a string literal" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_unmatched_entrypoint loc ->
    let message = `String "no constructor matches the entrypoint annotation" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_nested_big_map loc ->
    let message = `String "it looks like you have nested a big map inside another big map, this is not supported" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_corner_case desc ->
    let message = `String "internal error" in
    let description = `String desc in
    let content = `Assoc [
       ("message", message);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_contract_io (entrypoint, e) ->
    let message = `String "badly typed contract" in
    let description = `String "unexpected entrypoint type" in
    let entrypoint = `String entrypoint in
    let eptype = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e.type_expression) in
    let content = `Assoc [
       ("message", message);
       ("description", description);
       ("entrypoint", entrypoint);
       ("location", Location.to_yojson e.location);
       ("type", eptype);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_contract_list_ops (entrypoint, got, e) ->
    let entrypoint = `String entrypoint in
    let message = `String "badly typed contract" in
    let actual = `String (Format.asprintf "%a"
      Ast_typed.PP.type_expression {got with type_content= T_constant {language="Michelson";injection=Ligo_string.verbatim Stage_common.Constant.list_name;parameters=[{got with type_content=(Ast_typed.t_operation ()).type_content}]}}) in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression got) in
    let content = `Assoc [
       ("message", message);
       ("entrypoint", entrypoint);
       ("location", Location.to_yojson e.location);
       ("expected", expected);
       ("actual", actual);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_expected_same_entry (entrypoint,t1,t2,e) ->
    let entrypoint = `String entrypoint in
    let message = `String "badly typed contract" in
    let description = `String "expected storages" in
    let t1 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t1) in
    let t2 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t2) in
    let content = `Assoc [
       ("entrypoint", entrypoint);
       ("message", message);
       ("location", Location.to_yojson e.location);
       ("description", description);
       ("type1", t1);
       ("type2", t2);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_pair_in (loc,t) ->
    let ep = match t with `View -> "badly typed view" | `Contract -> "badly typed contract" in
    let message = `String ep in
    let description = `String "expected a pair as parameter" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_pair_out loc ->
    let message = `String "badly typed contract" in
    let description = `String "expected a pair as return type" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
  | `Self_ast_typed_obj_ligo loc ->
    let message = `String "unexpected Test primitive" in
    let description = `String "these Test primitive or type cannot be used in code to be compiled or run" in
    let content = `Assoc [
       ("message", message);
       ("location", Location.to_yojson loc);
       ("description", description);
       ]
    in
    json_error ~stage ~content
