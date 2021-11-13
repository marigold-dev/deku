open Types

type json = Yojson.Safe.t

let constant' = function
  | C_INT                -> `List [`String "C_INT"; `Null ]
  | C_UNIT               -> `List [`String "C_UNIT"; `Null ]
  | C_NEVER              -> `List [`String "C_NEVER"; `Null ]
  | C_NIL                -> `List [`String "C_NIL"; `Null ]
  | C_NOW                -> `List [`String "C_NOW"; `Null ]
  | C_IS_NAT             -> `List [`String "C_IS_NAT"; `Null ]
  | C_SOME               -> `List [`String "C_SOME"; `Null ]
  | C_NONE               -> `List [`String "C_NONE"; `Null ]
  | C_UNOPT              -> `List [`String "C_UNOPT"; `Null ]
  | C_UNOPT_WITH_ERROR   -> `List [`String "C_UNOPT_WITH_ERROR"; `Null ]
  | C_ASSERTION          -> `List [`String "C_ASSERTION"; `Null ]
  | C_ASSERTION_WITH_ERROR -> `List [`String "C_ASSERTION_WITH_ERROR"; `Null ]
  | C_ASSERT_SOME        -> `List [`String "C_ASSERT_SOME"; `Null ]
  | C_ASSERT_SOME_WITH_ERROR -> `List [`String "C_ASSERT_SOME_WITH_ERROR"; `Null ]
  | C_ASSERT_INFERRED    -> `List [`String "C_ASSERT_INFERRED"; `Null ]
  | C_FAILWITH           -> `List [`String "C_FAILWITH"; `Null ]
  | C_UPDATE             -> `List [`String "C_UPDATE"; `Null ]
  (* Loops *)
  | C_ITER               -> `List [`String "C_ITER"; `Null ]
  | C_FOLD_WHILE         -> `List [`String "C_FOLD_WHILE"; `Null ]
  | C_FOLD_CONTINUE      -> `List [`String "C_FOLD_CONTINUE"; `Null ]
  | C_FOLD_STOP          -> `List [`String "C_FOLD_STOP"; `Null ]
  | C_LOOP_LEFT          -> `List [`String "C_LOOP_LEFT"; `Null ]
  | C_LOOP_CONTINUE      -> `List [`String "C_LOOP_CONTINUE"; `Null ]
  | C_LOOP_STOP          -> `List [`String "C_LOOP_STOP"; `Null ]
  | C_FOLD               -> `List [`String "C_FOLD"; `Null ]
  | C_FOLD_LEFT          -> `List [`String "C_FOLD_LEFT"; `Null ]
  | C_FOLD_RIGHT         -> `List [`String "C_FOLD_RIGHT"; `Null ]
  (* MATH *)
  | C_NEG                -> `List [`String "C_NEG"; `Null ]
  | C_ABS                -> `List [`String "C_ABS"; `Null ]
  | C_ADD                -> `List [`String "C_ADD"; `Null ]
  | C_SUB                -> `List [`String "C_SUB"; `Null ]
  | C_MUL                -> `List [`String "C_MUL"; `Null ]
  | C_EDIV               -> `List [`String "C_EDIV"; `Null ]
  | C_DIV                -> `List [`String "C_DIV"; `Null ]
  | C_MOD                -> `List [`String "C_MOD"; `Null ]
  (* LOGIC *)
  | C_NOT                -> `List [`String "C_NOT"; `Null ]
  | C_AND                -> `List [`String "C_AND"; `Null ]
  | C_OR                 -> `List [`String "C_OR"; `Null ]
  | C_XOR                -> `List [`String "C_XOR"; `Null ]
  | C_LSL                -> `List [`String "C_LSL"; `Null ]
  | C_LSR                -> `List [`String "C_LSR"; `Null ]
  (* COMPARATOR *)
  | C_EQ                 -> `List [`String "C_EQ"; `Null ]
  | C_NEQ                -> `List [`String "C_NEQ"; `Null ]
  | C_LT                 -> `List [`String "C_LT"; `Null ]
  | C_GT                 -> `List [`String "C_GT"; `Null ]
  | C_LE                 -> `List [`String "C_LE"; `Null ]
  | C_GE                 -> `List [`String "C_GE"; `Null ]
  (* Bytes/ String *)
  | C_SIZE               -> `List [`String "C_SIZE"; `Null ]
  | C_CONCAT             -> `List [`String "C_CONCAT"; `Null ]
  | C_SLICE              -> `List [`String "C_SLICE"; `Null ]
  | C_BYTES_PACK         -> `List [`String "C_BYTES_PACK"; `Null ]
  | C_BYTES_UNPACK       -> `List [`String "C_BYTES_UNPACK"; `Null ]
  | C_CONS               -> `List [`String "C_CONS"; `Null ]
  (* Pair *)
  | C_PAIR               -> `List [`String "C_PAIR"; `Null ]
  | C_CAR                -> `List [`String "C_CAR"; `Null ]
  | C_CDR                -> `List [`String "C_CDR"; `Null ]
  | C_TRUE               -> `List [`String "C_TRUE"; `Null ]
  | C_FALSE              -> `List [`String "C_FALSE"; `Null ]
  | C_LEFT               -> `List [`String "C_LEFT"; `Null ]
  | C_RIGHT              -> `List [`String "C_RIGHT"; `Null ]
  (* Set *)
  | C_SET_EMPTY          -> `List [`String "C_SET_EMPTY"; `Null ]
  | C_SET_LITERAL        -> `List [`String "C_SET_LITERAL"; `Null ]
  | C_SET_ADD            -> `List [`String "C_SET_ADD"; `Null ]
  | C_SET_REMOVE         -> `List [`String "C_SET_REMOVE"; `Null ]
  | C_SET_ITER           -> `List [`String "C_SET_ITER"; `Null ]
  | C_SET_FOLD           -> `List [`String "C_SET_FOLD"; `Null ]
  | C_SET_FOLD_DESC     -> `List [`String "C_SET_FOLD_DESC"; `Null ]
  | C_SET_MEM            -> `List [`String "C_SET_MEM"; `Null ]
  | C_SET_UPDATE         -> `List [`String "C_SET_UPDATE"; `Null ]
  (* List *)
  | C_LIST_EMPTY         -> `List [`String "C_LIST_EMPTY"; `Null ]
  | C_LIST_LITERAL       -> `List [`String "C_LIST_LITERAL"; `Null ]
  | C_LIST_ITER          -> `List [`String "C_LIST_ITER"; `Null ]
  | C_LIST_MAP           -> `List [`String "C_LIST_MAP"; `Null ]
  | C_LIST_FOLD          -> `List [`String "C_LIST_FOLD"; `Null ]
  | C_LIST_FOLD_LEFT     -> `List [`String "C_LIST_FOLD_LEFT"; `Null ]
  | C_LIST_FOLD_RIGHT    -> `List [`String "C_LIST_FOLD_RIGHT"; `Null ]
  | C_LIST_HEAD_OPT      -> `List [`String "C_LIST_HEAD_OPT"; `Null ]
  | C_LIST_TAIL_OPT      -> `List [`String "C_LIST_TAIL_OPT"; `Null ]
  (* Maps *)
  | C_MAP                -> `List [`String "C_MAP"; `Null ]
  | C_MAP_EMPTY          -> `List [`String "C_MAP_EMPTY"; `Null ]
  | C_MAP_LITERAL        -> `List [`String "C_MAP_LITERAL"; `Null ]
  | C_MAP_GET            -> `List [`String "C_MAP_GET"; `Null ]
  | C_MAP_GET_FORCE      -> `List [`String "C_MAP_GET_FORCE"; `Null ]
  | C_MAP_ADD            -> `List [`String "C_MAP_ADD"; `Null ]
  | C_MAP_REMOVE         -> `List [`String "C_MAP_REMOVE"; `Null ]
  | C_MAP_UPDATE         -> `List [`String "C_MAP_UPDATE"; `Null ]
  | C_MAP_ITER           -> `List [`String "C_MAP_ITER"; `Null ]
  | C_MAP_MAP            -> `List [`String "C_MAP_MAP"; `Null ]
  | C_MAP_FOLD           -> `List [`String "C_MAP_FOLD"; `Null ]
  | C_MAP_MEM            -> `List [`String "C_MAP_MEM"; `Null ]
  | C_MAP_FIND           -> `List [`String "C_MAP_FIND"; `Null ]
  | C_MAP_FIND_OPT       -> `List [`String "C_MAP_FIND_OPT"; `Null ]
  | C_MAP_GET_AND_UPDATE -> `List [`String "C_MAP_GET_AND_UPDATE"; `Null ]
  (* Big Maps *)
  | C_BIG_MAP            -> `List [`String "C_BIG_MAP"; `Null ]
  | C_BIG_MAP_EMPTY      -> `List [`String "C_BIG_MAP_EMPTY"; `Null ]
  | C_BIG_MAP_LITERAL    -> `List [`String "C_BIG_MAP_LITERAL"; `Null ]
  | C_BIG_MAP_GET_AND_UPDATE -> `List [`String "C_BIG_MAP_GET_AND_UPDATE"; `Null ]
  (* Crypto *)
  | C_SHA256             -> `List [`String "C_SHA256"; `Null ]
  | C_SHA512             -> `List [`String "C_SHA512"; `Null ]
  | C_BLAKE2b            -> `List [`String "C_BLAKE2b"; `Null ]
  | C_HASH               -> `List [`String "C_HASH"; `Null ]
  | C_HASH_KEY           -> `List [`String "C_HASH_KEY"; `Null ]
  | C_CHECK_SIGNATURE    -> `List [`String "C_CHECK_SIGNATURE"; `Null ]
  | C_CHAIN_ID           -> `List [`String "C_CHAIN_ID"; `Null ]
  (* Blockchain *)
  | C_CALL                     -> `List [`String "C_CALL"; `Null ]
  | C_CONTRACT                 -> `List [`String "C_CONTRACT"; `Null ]
  | C_CONTRACT_WITH_ERROR      -> `List [`String "C_CONTRACT_WITH_ERROR"; `Null ]
  | C_CONTRACT_OPT             -> `List [`String "C_CONTRACT_OPT"; `Null ]
  | C_CONTRACT_ENTRYPOINT      -> `List [`String "C_CONTRACT_ENTRYPOINT"; `Null ]
  | C_CONTRACT_ENTRYPOINT_OPT  -> `List [`String "C_CONTRACT_ENTRYPOINT_OPT"; `Null ]
  | C_AMOUNT                   -> `List [`String "C_AMOUNT"; `Null ]
  | C_BALANCE                  -> `List [`String "C_BALANCE"; `Null ]
  | C_SOURCE                   -> `List [`String "C_SOURCE"; `Null ]
  | C_SENDER                   -> `List [`String "C_SENDER"; `Null ]
  | C_ADDRESS                  -> `List [`String "C_ADDRESS"; `Null ]
  | C_SELF                     -> `List [`String "C_SELF"; `Null ]
  | C_SELF_ADDRESS             -> `List [`String "C_SELF_ADDRESS"; `Null ]
  | C_IMPLICIT_ACCOUNT         -> `List [`String "C_IMPLICIT_ACCOUNT"; `Null ]
  | C_SET_DELEGATE             -> `List [`String "C_SET_DELEGATE"; `Null ]
  | C_CREATE_CONTRACT          -> `List [`String "C_CREATE_CONTRACT"; `Null ]
  | C_OPEN_CHEST               -> `List [`String "C_OPEN_CHEST"; `Null ]
  | C_VIEW                     -> `List [`String "C_VIEW"; `Null ]
  | C_TEST_ORIGINATE           -> `List [`String "TEST_ORIGINATE"; `Null ]
  | C_TEST_ORIGINATE_FROM_FILE -> `List [`String "TEST_ORIGINATE_FROM_FILE"; `Null ]
  | C_TEST_SET_NOW             -> `List [`String "TEST_SET_NOW"; `Null ]
  | C_TEST_SET_SOURCE          -> `List [`String "TEST_SET_SOURCE"; `Null ]
  | C_TEST_SET_BAKER           -> `List [`String "TEST_SET_BAKER"; `Null ]
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> `List [`String "TEST_EXTERNAL_CALL_TO_CONTRACT"; `Null ]
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> `List [`String "TEST_EXTERNAL_CALL_TO_CONTRACT_EXN"; `Null ]
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> `List [`String "TEST_EXTERNAL_CALL_TO_ADDRESS"; `Null ]
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN   -> `List [`String "TEST_EXTERNAL_CALL_TO_ADDRESS_EXN"; `Null ]
  | C_TEST_GET_STORAGE         -> `List [`String "TEST_GET_STORAGE"; `Null ]
  | C_TEST_GET_STORAGE_OF_ADDRESS -> `List [`String "TEST_GET_STORAGE_OF_ADDRESS"; `Null ]
  | C_TEST_GET_BALANCE         -> `List [`String "TEST_GET_BALANCE"; `Null ]
  | C_TEST_MICHELSON_EQUAL        -> `List [`String "TEST_ASSERT_FAILURE"; `Null ]
  | C_TEST_GET_NTH_BS          -> `List [`String "TEST_GET_NTH_BS"; `Null ]
  | C_TEST_LOG                 -> `List [`String "TEST_LOG"; `Null ]
  | C_TEST_STATE_RESET         -> `List [`String "TEST_STATE_RESET"; `Null ]
  | C_TEST_BOOTSTRAP_CONTRACT  -> `List [`String "TEST_BOOTSTRAP_CONTRACT"; `Null ]
  | C_TEST_NTH_BOOTSTRAP_CONTRACT  -> `List [`String "TEST_NTH_BOOTSTRAP_CONTRACT"; `Null ]
  | C_TEST_LAST_ORIGINATIONS   -> `List [`String "TEST_LAST_ORIGINATIONS"; `Null ]
  | C_TEST_COMPILE_META_VALUE  -> `List [`String "TEST_COMPILE_META_VALUE"; `Null ]
  | C_TEST_MUTATE_COUNT        -> `List [`String "TEST_MUTATE_COUNT"; `Null]
  | C_TEST_MUTATE_VALUE        -> `List [`String "TEST_MUTATE_VALUE"; `Null]
  | C_TEST_MUTATION_TEST       -> `List [`String "TEST_MUTATION_TEST"; `Null]
  | C_TEST_MUTATION_TEST_ALL   -> `List [`String "TEST_MUTATION_TEST_ALL"; `Null]
  | C_TEST_SAVE_MUTATION       -> `List [`String "TEST_SAVE_MUTATION"; `Null]
  | C_TEST_RUN                 -> `List [`String "TEST_RUN"; `Null ]
  | C_TEST_EVAL                -> `List [`String "TEST_EVAL"; `Null ]
  | C_TEST_COMPILE_CONTRACT    -> `List [`String "TEST_COMPILE_CONTRACT"; `Null ]
  | C_TEST_TO_CONTRACT         -> `List [`String "TEST_TO_CONTRACT"; `Null ]
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> `List [`String "TEST_NTH_BOOTSTRAP_TYPED_ADDRESS"; `Null ]
  | C_TEST_TO_ENTRYPOINT       -> `List [`String "TEST_TO_ENTRYPOINT"; `Null ]
  | C_TEST_TO_TYPED_ADDRESS    -> `List [`String "TEST_TO_TYPED_ADDRESS"; `Null ]
  | C_TEST_SET_BIG_MAP         -> `List [`String "TEST_SET_BIG_MAP"; `Null ]
  | C_TEST_CAST_ADDRESS        -> `List [`String "C_TEST_CAST_ADDRESS"; `Null ]
  | C_TEST_CREATE_CHEST        -> `List [`String "C_TEST_CREATE_CHEST"; `Null ]
  | C_TEST_CREATE_CHEST_KEY    -> `List [`String "C_TEST_CREATE_CHEST_KEY"; `Null ]
  | C_SHA3                     -> `List [`String "SHA3"; `Null ]
  | C_KECCAK                   -> `List [`String "KECCAK"; `Null ]
  | C_LEVEL                    -> `List [`String "LEVEL"; `Null ]
  | C_VOTING_POWER             -> `List [`String "VOTING_POWER"; `Null ]
  | C_TOTAL_VOTING_POWER       -> `List [`String "TOTAL_VOTING_POWER"; `Null ]
  | C_TICKET                   -> `List [`String "TICKET"; `Null ]
  | C_READ_TICKET              -> `List [`String "READ_TICKET"; `Null ]
  | C_SPLIT_TICKET             -> `List [`String "SPLIT_TICKET"; `Null ]
  | C_JOIN_TICKET              -> `List [`String "JOIN_TICKET"; `Null ]
  | C_PAIRING_CHECK            -> `List [`String "PAIRING_CHECK"; `Null ]
  | C_SAPLING_VERIFY_UPDATE    -> `List [`String "SAPLING_VERIFY_UPDATE"; `Null ]
  | C_SAPLING_EMPTY_STATE      -> `List [`String "SAPLING_EMPTY_STATE"; `Null ]
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD          -> `List [`String "POLYMORPHIC_ADD"; `Null ]

let literal = function
  | Literal_unit        -> `List [`String "Literal_unit"; `Null ]
  | Literal_int       l -> `List [`String "Literal_int"; z_to_yojson l ]
  | Literal_nat       l -> `List [`String "Literal_nat"; z_to_yojson l ]
  | Literal_timestamp l -> `List [`String "Literal_timestamp"; z_to_yojson l ]
  | Literal_mutez     l -> `List [`String "Literal_mutez"; z_to_yojson l ]
  | Literal_string    l -> `List [`String "Literal_string"; Ligo_string.to_yojson l ]
  | Literal_bytes     l -> `List [`String "Literal_bytes"; bytes_to_yojson l ]
  | Literal_address   l -> `List [`String "Literal_address"; `String l ]
  | Literal_signature l -> `List [`String "Literal_signature"; `String l ]
  | Literal_key       l -> `List [`String "Literal_key"; `String l ]
  | Literal_key_hash  l -> `List [`String "Literal_key_hash"; `String l ]
  | Literal_chain_id  l -> `List [`String "Literal_chaind_id"; `String l ]
  | Literal_operation l -> `List [`String "Literal_operation"; bytes_to_yojson l ]

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
