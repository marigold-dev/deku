open Types

type json = Yojson.Safe.t

let constant' = constant'_to_yojson

let literal = literal_to_yojson

let label (Label l) = `List [`String "Label"; `String l]
let option f o =
  match o with
  | None   -> `List [ `String "None" ; `Null ]
  | Some v -> `List [ `String "Some" ; f v ]

let option' f o =
  match o with
  | None -> `Null
  | Some v -> f v

let string s = `String s

let list f lst = `List (List.map ~f:f lst)

let label_map f lmap =
  let lst = List.sort ~compare:(fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
  let lst' = List.fold_left
      ~f:(fun acc (Label k, v) -> (k , f v)::acc)
      ~init:[] lst
  in
  `Assoc lst'

let attributes attr =
  let list = List.map ~f:(fun string -> `String string) attr
  in `Assoc [("attributes", `List list)]

let for_all type_expression {ty_binder ; kind = _ ; type_ } =
  `Assoc [
    ("ty_binder", Location.wrap_to_yojson type_variable_to_yojson ty_binder) ;
    (* ("kind", ) *)
    ("type_", type_expression type_)
  ]

let binder type_expression {var;ascr;attributes} =
  let attributes = match attributes.const_or_var with
        | None -> []
        | Some `Var -> [("const_or_var", `String "var")]
        | Some `Const -> [("const_or_var", `String "const")] in
  `Assoc ([
    ("var", expression_variable_to_yojson var);
    ("ty", option' type_expression ascr);
    ] @ attributes)

let row_element g {associated_type; michelson_annotation; decl_pos} =
  `Assoc [
    ("associated_type", g associated_type);
    ("michelson_annotation", option (fun s -> `String s) michelson_annotation);
    ("decl_pos", `Int decl_pos);
  ]

let module_access f {module_name;element} =
  `Assoc [
    ("module_name", module_variable_to_yojson module_name) ;
    ("element", f element) ;
  ]
let t_app f {type_operator ; arguments } =
  `Assoc [
    ("type_operator", type_variable_to_yojson type_operator) ;
    ("arguments", (list f arguments))
  ]

let arrow type_expression {type1;type2} =
  `Assoc [
    ("type1", type_expression type1);
    ("type2", type_expression type2);
  ]

let constant expression {cons_name;arguments} =
  `Assoc [
    ("cons_name", constant' cons_name);
    ("arguments", list expression arguments);
  ]

let application expression {lamb;args} =
  `Assoc [
    ("lamb", expression lamb);
    ("args", expression args);
  ]

let lambda expression type_expression {binder=b;output_type;result} : json =
  `Assoc [
    ("binder", binder type_expression b);
    ("output_type", option' type_expression output_type);
    ("result", expression result);
  ]

let recursive expression type_expression {fun_name;fun_type;lambda=l} =
  `Assoc [
    ("fun_name", expression_variable_to_yojson fun_name);
    ("fun_type", type_expression fun_type);
    ("lambda", lambda expression type_expression l)
  ]

let let_in expression type_expression {let_binder;rhs;let_result;attributes=attr} =
  `Assoc [
    ("let_binder", binder type_expression let_binder);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("attributes", attributes attr);
  ]

let type_in expression type_expression {type_binder;rhs;let_result} =
  `Assoc [
    ("let_binder", type_variable_to_yojson type_binder );
    ("rhs", type_expression rhs);
    ("let_result", expression let_result)
  ]

let raw_code expression {language;code} =
  `Assoc [
    ("language", `String language);
    ("code", expression code);
  ]

let constructor expression {constructor;element} =
  `Assoc [
    ("constructor", label constructor);
    ("element", expression element);
  ]

let record expression r = label_map expression r

let access expression = function
  | Access_tuple  a -> `List [ `String "Access_tuple"; z_to_yojson a]
  | Access_record a -> `List [ `String "Access_tuple"; `String a]
  | Access_map    a -> `List [ `String "Access_tuple"; expression a]

let accessor expression ({record; path}: 'exp accessor) =
  `Assoc [
    ("record", expression record);
    ("path", list (access expression) path);
  ]

let record_accessor expression ({record; path}: 'exp record_accessor) =
  `Assoc [
    ("record", expression record);
    ("path", label path);
  ]

let update expression ({record; path; update}: 'exp update) =
  `Assoc [
    ("record", expression record);
    ("path", list (access expression) path);
    ("update", expression update);
  ]

let record_update expression {record; path; update} =
  `Assoc [
    ("record", expression record);
    ("path", label path);
    ("update", expression update);
  ]

let ascription expression type_expression {anno_expr; type_annotation} =
  `Assoc [
    ("anno_expr", expression anno_expr);
    ("type_annotation", type_expression type_annotation);
  ]
let conditional expression {condition; then_clause; else_clause} =
  `Assoc [
    ("condition", expression condition);
    ("then_clause", expression then_clause);
    ("else_clause", expression else_clause);
  ]
let sequence expression {expr1;expr2} =
  `Assoc [
    ("expr1", expression expr1);
    ("expr2", expression expr2);
  ]

let assign expression {variable; access_path; expression=e} =
  `Assoc [
    ("variable", expression_variable_to_yojson variable);
    ("access_path", list (access expression) access_path);
    ("expression", expression e);
  ]

let for_ expression {binder; start; final; incr; f_body} =
  `Assoc [
    ("binder", expression_variable_to_yojson binder);
    ("start" , expression start);
    ("final" , expression final);
    ("incr"  , expression incr);
    ("f_body", expression f_body);
  ]

let collect_type = function
  | Map  -> `List [ `String "Map"; `Null]
  | Set  -> `List [ `String "Set"; `Null]
  | List -> `List [ `String "List"; `Null]
  | Any  -> `List [ `String "Any"; `Null]

let for_each expression {fe_binder;collection;collection_type;fe_body} =
  `Assoc [
    ("binder",  `List [ expression_variable_to_yojson @@ fst fe_binder; option expression_variable_to_yojson @@ snd fe_binder]);
    ("collection", expression collection);
    ("collection_type", collect_type collection_type);
    ("body", expression fe_body);
  ]

let while_loop expression {cond;body} =
  `Assoc [
    ("cond", expression cond);
    ("body", expression body);
  ]

let rec list_pattern type_expression lp =
  match lp with
  | Cons (a,b) -> `List [`String "Cons" ; pattern type_expression a ; pattern type_expression b]
  | List lp -> `List [`String "Tuple" ; list (pattern type_expression) lp ]

and pattern type_expression p =
  match p.wrap_content with
  | P_unit -> `List [`String "Unit" ; `Null]
  | P_var b -> `List [`String "Var"; binder type_expression b]
  | P_list lp -> `List [`String "List" ; list_pattern type_expression lp]
  | P_variant (l,p) -> `List [`String "Variant" ; label l ; (pattern type_expression) p]
  | P_tuple lp -> `List [`String "Tuple" ; list (pattern type_expression) lp]
  | P_record (ll,lp) -> `List [`String "Record" ; list label ll ; list (pattern type_expression) lp]

and match_case expression type_expression {pattern=p ; body } =
  `Assoc [
    ("pattern", pattern type_expression p) ;
    ("body", expression body) ;
  ]

let match_exp expression type_expression {matchee ; cases} =
  `Assoc [
    ("matchee", expression matchee) ;
    ("cases", list (match_case expression type_expression) cases) ;
  ]

let declaration_type type_expression {type_binder; type_expr; type_attr} =
  `Assoc [
    ("type_binder", type_variable_to_yojson type_binder);
    ("type_expr", type_expression type_expr);
    ("type_attr", attributes type_attr)
  ]

let declaration_constant expression type_expression {name; binder=b;attr;expr} =
  `Assoc [
    ("name", option' string name);
    ("binder", binder type_expression b);
    ("expr", expression expr);
    ("attribute", attributes attr);
  ]

let rec declaration_module expression type_expression {module_binder;module_;module_attr} =
  `Assoc [
    ("module_binder", module_variable_to_yojson module_binder);
    ("module_", (module' expression type_expression) module_);
    ("module_attr", attributes module_attr);
  ]

and module_alias ({alias;binders} : module_alias) =
  `Assoc [
    ("alias"  , module_variable_to_yojson alias) ;
    ("binders", list module_variable_to_yojson @@ List.Ne.to_list binders) ;
  ]

and declaration expression type_expression = function
  Declaration_type    ty -> `List [ `String "Declaration_type"    ; declaration_type                type_expression ty ]
| Declaration_constant c -> `List [ `String "Declaration_constant"; declaration_constant expression type_expression c  ]
| Declaration_module   m -> `List [ `String "Declaration_module"  ; declaration_module   expression type_expression m  ] 
| Module_alias        ma -> `List [ `String "Module_alias"        ; module_alias                                    ma ]

and module' expression type_expression = list (Location.wrap_to_yojson (declaration expression type_expression))


and mod_in expression type_expression {module_binder;rhs;let_result} =
  `Assoc [
    ("module_binder", module_variable_to_yojson module_binder );
    ("rhs", (module' expression type_expression) rhs);
    ("let_result", expression let_result);
  ]

and mod_alias expression {alias; binders; result} =
  `Assoc [
    ("alias",  module_variable_to_yojson alias  );
    ("binders", list module_variable_to_yojson @@ List.Ne.to_list binders );
    ("result", expression result );
  ]
