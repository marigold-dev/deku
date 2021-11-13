open Types
open Stage_common.To_yojson
type json = Yojson.Safe.t
type 'a json_printer = 'a -> json

let constant' = Stage_common.To_yojson.constant'

let label = label_to_yojson
let option f o =
  match o with
  | None   -> `List [ `String "None" ; `Null ]
  | Some v -> `List [ `String "Some" ; f v ]

let pair f g (x, y) = `Tuple [ f x ; g y ]
let list f lst = `List (List.map ~f:f lst)
let label_map f lmap =
  let lst = List.sort ~compare:(fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
  let lst' = List.fold_left
      ~f:(fun acc (Label k, v) -> (k , f v)::acc)
      ~init:[] lst
  in
  `Assoc lst'

let layout = function
  | L_comb -> `List [ `String "L_comb"; `Null ]
  | L_tree -> `List [ `String "L_tree"; `Null ]


let rec type_expression {type_content=tc;type_meta;location;orig_var} =
  `Assoc [
    ("type_content", type_content tc);
    ("type_meta", option Ast_core.Yojson.type_expression type_meta);
    ("location", Location.to_yojson location);
    ("orig_var", option type_variable_to_yojson orig_var);
  ]

and type_content = function
  | T_variable        t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_sum             t -> `List [ `String "t_sum"; rows t]
  | T_record          t -> `List [ `String "t_record"; rows t]
  | T_arrow           t -> `List [ `String "t_arrow"; arrow t]
  | T_constant        t -> `List [ `String "t_constant"; type_injection t]
  | T_module_accessor t -> `List [ `String "t_module_accessor"; module_access type_expression t]
  | T_singleton       t -> `List [ `String "t_singleton" ; literal t ]
  | T_abstraction         t -> `List [ `String "t_abstraction" ; for_all type_expression t]
  | T_for_all         t -> `List [ `String "t_for_all" ; for_all type_expression t]

and type_injection {language;injection;parameters} =
  `Assoc [
    ("language", `String language);
    ("injection", `String (Ligo_string.extract injection));
    ("parameters", list type_expression parameters)
  ]

and rows {content; layout = l } =
  `Assoc [
    ("content", label_map row_element content);
    ("layout", layout l);
  ]
and row_element {associated_type; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_type", type_expression associated_type);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]

and arrow {type1;type2} =
  `Assoc [
    ("type1", type_expression type1);
    ("type2", type_expression type2);
  ]

let rec expression {expression_content=ec;location;type_expression=te} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
    ("type_expression", type_expression te)
  ]

and expression_content = function
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal"; Stage_common.To_yojson.literal e ]
  | E_constant    e -> `List [ `String "E_constant"; constant e ]
  | E_variable    e -> `List [ `String "E_variable"; expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application e ]
  | E_lambda      e -> `List [ `String "E_lambda"; lambda e ]
  | E_recursive   e -> `List [ `String "E_recursive"; recursive e ]
  | E_let_in      e -> `List [ `String "E_let_in"; let_in e ]
  | E_type_in     e -> `List [ `String "E_type_in"; type_in e ]
  | E_mod_in      e -> `List [ `String "E_mod_in"; mod_in e ]
  | E_mod_alias   e -> `List [ `String "E_mod_alias"; mod_alias expression e ]
  | E_raw_code    e -> `List [ `String "E_raw_code"; raw_code e ]
  (* Variant *)
  | E_constructor     e -> `List [ `String "E_constructor"; constructor e ]
  | E_matching        e -> `List [ `String "E_matching"; matching e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record"; record e ]
  | E_record_accessor e -> `List [ `String "E_record_accessor"; record_accessor e ]
  | E_record_update   e -> `List [ `String "E_record_update"; record_update e ]
  | E_module_accessor e -> `List [ `String "E_module_accessor"; module_access expression e]
  | E_type_inst       e -> `List [ `String "E_type_inst"; type_inst e ]

and constant {cons_name;arguments} =
  `Assoc [
    ("cons_name", constant' cons_name);
    ("arguments", list expression arguments);
  ]

and type_inst {forall;type_} =
  `Assoc [
    ("forall", expression forall);
    ("type_", type_expression type_);
  ]

and application {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

and lambda {binder;result} =
  `Assoc [
    ("binder", expression_variable_to_yojson binder);
    ("result", expression result);
  ]

and recursive {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", expression_variable_to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda l)
  ]

and attribute {inline;no_mutation;public;view} =
  `Assoc [
    ("inline", `Bool inline);
    ("no_mutation", `Bool no_mutation);
    ("view", `Bool view);
    ("public", `Bool public);
  ]

and type_attribute ({public}: type_attribute) =
  `Assoc [
    ("public", `Bool public)
  ]

and module_attribute ({public}: module_attribute) =
  `Assoc [
    ("public", `Bool public)
  ]


and let_in {let_binder;rhs;let_result;attr} =
  `Assoc [
    ("let_binder", expression_variable_to_yojson let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("attr", attribute attr);
  ]

and type_in {type_binder;rhs;let_result} =
  `Assoc [
    ("let_binder", type_variable_to_yojson type_binder );
    ("rhs", type_expression rhs);
    ("let_result", expression let_result)
  ]


and mod_in {module_binder;rhs;let_result} =
  `Assoc [
    ("module_binder", module_variable_to_yojson module_binder);
    ("rhs", module_fully_typed rhs);
    ("let_result", expression let_result);
  ]

and raw_code {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

and constructor {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

and matching {matchee; cases} =
  `Assoc [
    ("matchee", expression matchee);
    ("cases", matching_expr cases);
  ]

and record r = label_map expression r

and record_accessor {record; path} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
  ]

and record_update {record; path; update} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
    ("update", expression update);
  ]

and matching_expr = function
  | Match_variant m -> `List [ `String "Match_variant"; matching_content_variant m ]
  | Match_record m -> `List [ `String "Match_record"; matching_content_record m ]

and matching_content_variant {cases;tv} =
  `Assoc [
    ("cases", list matching_content_case cases);
    ("tv", type_expression tv);
  ]

and matching_content_case {constructor; pattern; body} =
  `Assoc [
    ("constructor", label constructor);
    ("pattern", expression_variable_to_yojson pattern);
    ("body", expression body);
  ]

and matching_content_record {fields; body; tv} =
  `Assoc [
    ("fields", label_map (pair expression_variable_to_yojson type_expression) fields);
    ("body", expression body);
    ("record_type", type_expression tv);
  ]

and declaration_type {type_binder;type_expr; type_attr} =
  `Assoc [
    ("type_binder", type_variable_to_yojson type_binder);
    ("type_expr", type_expression type_expr);
    ("type_attr", type_attribute type_attr);
  ]

and declaration_constant {name; binder;expr;attr} =
  `Assoc [
    ("name", option' string name);
    ("binder",expression_variable_to_yojson binder);
    ("expr", expression expr);
    ("attr", attribute attr);
  ]

and declaration_module {module_binder;module_; module_attr} =
  `Assoc [
    ("module_binder",module_variable_to_yojson module_binder);
    ("module_", module_fully_typed module_);
    ("module_attr", module_attribute module_attr)
  ]

and module_alias ({alias ; binders} : module_alias) =
  `Assoc [
    ("alais"  , module_variable_to_yojson alias);
    ("binders", list module_variable_to_yojson @@ List.Ne.to_list binders);
  ]
and declaration = function
  | Declaration_type     dt -> `List [ `String "Declaration_type";     declaration_type dt]
  | Declaration_constant dc -> `List [ `String "Declaration_constant"; declaration_constant dc]
  | Declaration_module   dm -> `List [ `String "Declaration_module";   declaration_module dm]
  | Module_alias         ma -> `List [ `String "Module_alias";         module_alias ma]

and module_fully_typed (Module_Fully_Typed p) = list (Location.wrap_to_yojson declaration) p
let module_with_unification_vars (Module_With_Unification_Vars p) = list (Location.wrap_to_yojson declaration) p


(* Environment *)

let environment_element_definition_declaration {expression=e; free_variables ; attr = _} =
  `Assoc [
    ("expression", expression e);
    ("free_variables", list expression_variable_to_yojson free_variables);
  ]

let environment_element_definition = function
  | ED_binder  -> `List [ `String "ED_binder"; `Null]
  | ED_declaration ed -> `List [ `String "ED_declaration"; environment_element_definition_declaration ed]

let rec environment_element {type_value;definition} =
  `Assoc [
    ("type_value", type_expression type_value);
    ("definition", environment_element_definition definition);
  ]

and environment_binding {expr_var;env_elt;public} =
  `Assoc [
    ("expr_var", expression_variable_to_yojson expr_var);
    ("env_elt", environment_element env_elt);
    ("public", `Bool public)
  ]
and expression_environment e = list environment_binding e

and type_or_kind x =
  match x with
  | Ty ty -> `List [ `String "Ty"; type_expression ty]
  | Kind () -> `List [ `String "Kind"; `Null ]

and type_environment_binding {type_variable;type_;public} =
  `Assoc [
    ("type_variable", type_variable_to_yojson type_variable);
    ("type_", type_or_kind type_);
    ("public", `Bool public)
  ]
and type_environment e = list type_environment_binding e

and module_environment_binding {module_variable; module_; public} =
  `Assoc [
    ("module_name", module_variable_to_yojson module_variable);
    ("module_", environment module_);
    ("public", `Bool public)
  ]

and module_environment e = list module_environment_binding e

and environment {expression_environment=ee;type_environment=te;module_environment=me} =
  `Assoc [
    ("expression_environment", expression_environment ee);
    ("type_environment", type_environment te);
    ("module_environment", module_environment me)
  ]

(* Solver types *)

let constant_tag : constant_tag -> json = function
  | C_arrow        -> `List [`String "C_arrow"; `Null]
  | C_option       -> `List [`String "C_option"; `Null]
  | C_map          -> `List [`String "C_map"; `Null]
  | C_big_map      -> `List [`String "C_big_map"; `Null]
  | C_list         -> `List [`String "C_list"; `Null]
  | C_set          -> `List [`String "C_set"; `Null]
  | C_unit         -> `List [`String "C_unit"; `Null]
  | C_string       -> `List [`String "C_string"; `Null]
  | C_nat          -> `List [`String "C_nat"; `Null]
  | C_mutez        -> `List [`String "C_mutez"; `Null]
  | C_timestamp    -> `List [`String "C_timestamp"; `Null]
  | C_int          -> `List [`String "C_int"; `Null]
  | C_address      -> `List [`String "C_address"; `Null]
  | C_bytes        -> `List [`String "C_bytes"; `Null]
  | C_key_hash     -> `List [`String "C_key_hash"; `Null]
  | C_key          -> `List [`String "C_key"; `Null]
  | C_signature    -> `List [`String "C_signature"; `Null]
  | C_operation    -> `List [`String "C_operation"; `Null]
  | C_contract     -> `List [`String "C_contract"; `Null]
  | C_chain_id     -> `List [`String "C_chain_id"; `Null]
  | C_bls12_381_g1 -> `List [`String "C_bls12_381_g1"; `Null]
  | C_bls12_381_g2 -> `List [`String "C_bls12_381_g2"; `Null]
  | C_bls12_381_fr -> `List [`String "C_bls12_381_fr"; `Null]

let row_tag = function
  | C_record  -> `List [`String "C_record"; `Null]
  | C_variant -> `List [`String "C_variant"; `Null]
