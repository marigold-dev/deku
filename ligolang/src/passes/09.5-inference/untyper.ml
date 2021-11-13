module I = Ast_core
module O = Ast_core
open Stage_common.Maps

let untype_type_value (t:O.type_expression) : I.type_expression =
  t

(*
  Tranform a Ast_inferred type_expression into an ast_core type_expression
*)
let rec untype_type_expression (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {fields ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       v' in
     let x' = O.LMap.map aux fields in
     return @@ I.T_sum { fields = x' ; layout }
  | O.T_record {fields;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = O.LMap.map aux fields in
    return @@ I.T_record {fields = x' ; layout}
  )
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_arrow arr ->
    let arr = arrow self arr in
    return @@ I.T_arrow arr
  | O.T_module_accessor ma ->
    let ma = module_access self ma in
    return @@ I.T_module_accessor ma
  | O.T_singleton x ->
    return @@ I.T_singleton x
  | O.T_app {type_operator;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ I.T_app {type_operator;arguments}
  | O.T_abstraction x ->
    let type_ = self x.type_ in
    return @@ I.T_abstraction {x with type_}
  | O.T_for_all x ->
    let type_ = self x.type_ in
    return @@ I.T_for_all {x with type_}

(*
  Transform a Ast_inferred expression into an ast_core expression
*)
let rec untype_expression (e:O.expression) : I.expression =
  let open I in
  let return e = I.make_e e in
  match e.expression_content with
  | E_variable n -> return @@ E_variable (Location.map Var.todo_cast n)
  | E_literal l -> return @@ E_literal l
  | E_constant {cons_name;arguments} ->
    let arguments = List.map ~f:untype_expression arguments in
    return @@ E_constant {cons_name;arguments}
  | E_lambda lambda ->
    let lambda = untype_lambda lambda in
    return @@ E_lambda lambda
  | E_application {lamb;args} ->
    let lamb = untype_expression lamb in
    let args = untype_expression args in
    return @@ E_application {lamb;args}
  | E_constructor {constructor; element} ->
    let element = untype_expression element in
    return @@ E_constructor {constructor; element}
  | E_record r ->
    let r' = O.LMap.map untype_expression r in
    return @@ E_record r'
  | E_record_accessor {record; path} ->
    let record = untype_expression record in
    return @@ E_record_accessor {record; path}
  | E_record_update {record; path; update} ->
    let record = untype_expression record in
    let update = untype_expression update in
    return @@ E_record_update {record; path; update}
  | E_matching {matchee;cases} -> (
    return @@ E_matching {matchee;cases}
  )
  | E_let_in {let_binder; rhs; let_result; attr} ->
    let rhs        = untype_expression rhs in
    let let_result = untype_expression let_result in
    let let_binder = Stage_common.Maps.binder untype_type_expression let_binder in
    return @@ E_let_in {let_binder; rhs; let_result; attr}
  | E_type_in {type_binder; rhs; let_result} ->
    let rhs = untype_type_expression rhs in
    let let_result = untype_expression let_result in
    return @@ E_type_in {type_binder; rhs; let_result}
  | E_mod_in {module_binder; rhs;let_result} ->
    let rhs        = untype_module_fully_inferred rhs in
    let let_result = untype_expression let_result in
    return @@ E_mod_in {module_binder; rhs; let_result}
  | E_mod_alias ma ->
    let ma = mod_alias untype_expression ma in
    return @@ E_mod_alias ma
  | E_raw_code {language; code} ->
    let code = untype_expression code in
    return @@ E_raw_code {language; code}
  | E_recursive {fun_name; fun_type; lambda} ->
    let lambda = untype_lambda lambda in
    let fun_type = untype_type_expression fun_type in
    let fun_name = Location.map Var.todo_cast fun_name in
    return @@ E_recursive {fun_name; fun_type; lambda}
  | E_module_accessor ma ->
    let ma = module_access untype_expression ma in
    return @@ E_module_accessor ma
  | E_ascription {anno_expr;type_annotation} ->
    let anno_expr = untype_expression anno_expr in
    let type_annotation = untype_type_expression type_annotation in
    return @@ E_ascription {anno_expr;type_annotation}

and untype_lambda {binder; output_type; result} : _ O.lambda =
    let binder = Stage_common.Maps.binder untype_type_expression binder in
    let output_type = Option.map ~f:untype_type_expression output_type in
    let result = untype_expression result in
    ({binder; output_type; result}: _ I.lambda)

(*
  Transform a Ast_inferred matching into an ast_core matching
*)

and untype_declaration : O.declaration -> I.declaration =
let return (d: I.declaration) = d in
function
  Declaration_type {type_binder; type_expr; type_attr} ->
  let type_expr = untype_type_expression type_expr in
  return @@ Declaration_type {type_binder; type_expr; type_attr}
| Declaration_constant {name; binder;expr;attr} ->
  let binder = Stage_common.Maps.binder untype_type_expression binder in
  let expr = untype_expression expr in
  return @@ Declaration_constant {name; binder;expr;attr}
| Declaration_module {module_binder;module_;module_attr} ->
  let module_ = untype_module_fully_inferred module_ in
  return @@ Declaration_module {module_binder;module_;module_attr}
| Module_alias ma ->
  return @@ Module_alias ma

and untype_module_fully_inferred : O.module_ -> I.module_ = fun (m) ->
  List.map ~f:(Location.map untype_declaration) m
