module I = Ast_core
module O = Ast_typed

(*
  Transform a Ast_typed type_expression into an ast_core type_expression
 *)
let rec untype_type_expression_nofail (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression_nofail in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let associated_type = untype_type_expression_nofail associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       v' in
     let x' = Stage_common.Types.LMap.map aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = untype_type_expression_nofail associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = Stage_common.Types.LMap.map aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_arrow {type1;type2} ->
    let arr = O.{type1 = self type1; type2 = self type2} in
    return @@ T_arrow arr
  | O.T_constant {language;injection;parameters} ->
    ignore language ;
    let arguments = List.map ~f:untype_type_expression_nofail parameters in
    let type_operator = Var.of_name (Ligo_string.extract injection) in
    return @@ I.T_app {type_operator;arguments}
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_module_accessor {module_name;element} ->
    let ma = O.{module_name; element = self element} in
    return @@ I.T_module_accessor ma
  | O.T_singleton x -> return @@ I.T_singleton x
  | O.T_abstraction x ->
    let type_ = untype_type_expression_nofail x.type_ in
    return @@ I.T_abstraction {x with type_}
  | O.T_for_all x ->
    let type_ = untype_type_expression_nofail x.type_ in
    return @@ I.T_for_all {x with type_}

let untype_type_expression (t:O.type_expression) : I.type_expression =
  untype_type_expression_nofail t

let untype_declaration_constant untype_expression O.{name;binder;expr;attr} =
  let ty = untype_type_expression expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let binder = ({var;ascr= Some ty;attributes=Stage_common.Helpers.empty_attribute}: _ I.binder) in
  let expr = untype_expression expr in
  let expr = I.e_ascription expr ty in
  I.{name;binder;attr;expr;}

let untype_declaration_type O.{type_binder; type_expr; type_attr={public}} =
  let type_expr = untype_type_expression type_expr in
  let type_binder = Var.todo_cast type_binder in
  let type_attr = (I.{public}: I.type_attribute) in
  I.{type_binder; type_expr; type_attr}

let rec untype_declaration_module untype_expression O.{module_binder; module_; module_attr={public}} =
  let module_ = untype_module untype_expression module_ in
  let module_attr = (I.{public}: I.module_attribute) in
  I.{module_binder; module_ = module_; module_attr}

and untype_declaration untype_expression =
  let return (d: I.declaration) = d in
  fun (d: O.declaration) -> match d with
  | Declaration_constant dc ->
    let dc = untype_declaration_constant untype_expression dc in
    return @@ Declaration_constant dc
  | Declaration_type dt ->
    let dt = untype_declaration_type dt in
    return @@ Declaration_type dt
  | Declaration_module dm ->
    let dm = untype_declaration_module untype_expression dm in
    return @@ Declaration_module dm
  | Module_alias ma -> 
    return @@ Module_alias ma



and untype_module untype_expression : O.module_fully_typed -> I.module_ =
  fun (O.Module_Fully_Typed p) ->
  let untype_declaration = untype_declaration untype_expression in
  List.map ~f:(Location.map untype_declaration) p
