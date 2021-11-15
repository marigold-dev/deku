open Types
include Stage_common.To_yojson


let deprecated {name;const} =
  `Assoc [
    ("name", `String name);
    ("const", constant' const);
  ]

let rich_constant = function
  | Deprecated d -> `List [`String "Deprecatd"; deprecated d ]
  | Const      c -> `List [`String "Const"; constant' c ]


let rec type_expression {type_content=tc;location} =
  `Assoc [
    ("type_content", type_content tc);
    ("location", Location.to_yojson location);
  ]

and type_content = function
  | T_variable        t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_sum             t -> `List [ `String "t_sum";
                                   label_map row_element t.fields;
                                   attributes t.attributes]
  | T_record          t -> `List [ `String "t_record";
                                   label_map row_element t.fields;
                                   attributes t.attributes]
  | T_tuple           t -> `List [ `String "t_tuple";  list type_expression t]
  | T_arrow           t -> `List [ `String "t_arrow"; arrow type_expression t]
  | T_annoted         t -> `List [ `String "t_annoted"; `List [type_expression @@ fst t;`String (snd t)]]
  | T_app             t -> `List [ `String "t_app"; t_app type_expression t]
  | T_module_accessor t -> `List [ `String "t_module_accessor"; module_access type_expression t]
  | T_singleton       t -> `List [ `String "t_singleton" ; literal t ]
  | T_abstraction     t -> `List [ `String "t_abstraction" ; for_all type_expression t ]
  | T_for_all         t -> `List [ `String "t_for_all" ; for_all type_expression t ]

and row_element {associated_type; attributes=attr; decl_pos} =
  `Assoc [
    ("associated_type", type_expression associated_type);
    ("attributes", attributes attr);
    ("decl_pos", `Int decl_pos);
  ]

let rec expression {expression_content=ec;location} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
  ]

and expression_content = function
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal";     literal e ]
  | E_constant    e -> `List [ `String "E_constant";    constant e ]
  | E_variable    e -> `List [ `String "E_variable";    expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application expression e ]
  | E_lambda      e -> `List [ `String "E_lambda";      lambda    expression type_expression e ]
  | E_recursive   e -> `List [ `String "E_recursive";   recursive expression type_expression e ]
  | E_let_in      e -> `List [ `String "E_let_in";      let_in    expression type_expression e ]
  | E_type_in     e -> `List [ `String "E_type_in";     type_in   expression type_expression e ]
  | E_mod_in      e -> `List [ `String "E_mod_in";      mod_in    expression type_expression e ]
  | E_mod_alias   e -> `List [ `String "E_mod_alias";   mod_alias expression                 e ]
  | E_raw_code    e -> `List [ `String "E_raw_code";    raw_code  expression e ]
  (* Variant *)
  | E_constructor e -> `List [ `String "E_constructor"; constructor expression e ]
  | E_matching    e -> `List [ `String "E_matching";    match_exp expression type_expression e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record";          record      expression e ]
  | E_accessor        e -> `List [ `String "E_record_accessor"; accessor    expression e ]
  | E_update          e -> `List [ `String "E_record_update";   update      expression e ]
  | E_ascription      e -> `List [ `String "E_ascription";      ascription  expression type_expression e ]
  | E_module_accessor e -> `List [ `String "E_module_accessor"; module_access expression e]
  | E_cond            e -> `List [ `String "E_cond";            conditional expression e ]
  | E_sequence        e -> `List [ `String "E_sequence";        sequence    expression e ]
  | E_skip              -> `List [ `String "E_skip"; `Null ]
  | E_tuple           e -> `List [ `String "E_tuple"; list expression e ]
  (* Data Structures *)
  | E_map         e -> `List [ `String "E_map"; list (fun (k,v) -> `List [ expression k; expression v]) e ]
  | E_big_map     e -> `List [ `String "E_big_map"; list (fun (k,v) -> `List [ expression k; expression v]) e ]
  | E_list        e -> `List [ `String "E_list"; list expression e]
  | E_set         e -> `List [ `String "E_set"; list expression e]
  | E_assign      e -> `List [ `String "E_assign";   assign expression e ]
  | E_for         e -> `List [ `String "E_for";      for_   expression e ]
  | E_for_each    e -> `List [ `String "E_for_each"; for_each   expression e ]
  | E_while       e -> `List [ `String "E_while";    while_loop expression e ]


and constant {cons_name;arguments} =
  `Assoc [
    ("cons_name", rich_constant cons_name);
    ("arguments", list expression arguments);
  ]

and matching_content_cons (hd, tl, body) =
  `Assoc [
    ("hd", expression_variable_to_yojson hd);
    ("tl", expression_variable_to_yojson tl);
    ("body", expression body);
  ]

and matching_content_some (opt, body ) =
  `Assoc [
    ("opt", expression_variable_to_yojson opt);
    ("body", expression body);
  ]

and matching_content_case ((constructor, pattern), body) =
  `Assoc [
    ("constructor", label constructor);
    ("pattern", expression_variable_to_yojson pattern);
    ("body", expression body);
  ]

let declaration = declaration expression type_expression

let module_ = module' expression type_expression
