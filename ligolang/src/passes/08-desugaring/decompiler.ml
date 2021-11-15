module I = Ast_sugar
module O = Ast_core

open Stage_common.Maps

let cast_var = Location.map Var.todo_cast

let rec decompile_type_expression : O.type_expression -> I.type_expression =
  fun te ->
  let self = decompile_type_expression in
  let return te = I.make_t te in
  match te.sugar with
    Some te -> te
  | None ->
    match te.type_content with
      | O.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
      | O.T_app tc ->
        let tc = type_app self tc in
        return @@ T_app tc
      | O.T_sum {fields;layout} ->
        let fields =
          O.LMap.map (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ I.row_element = {associated_type;attributes;decl_pos} in
            v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" O.PP.layout l))] | None -> [] in
        return @@ I.T_sum {fields ; attributes}
      | O.T_record {fields;layout} ->
        let fields =
          O.LMap.map (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ I.row_element = {associated_type ; attributes ; decl_pos} in
            v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" O.PP.layout l))] | None -> [] in
        return @@ I.T_record { fields ; attributes }
      | O.T_arrow arr ->
        let arr = arrow self arr in
        return @@ T_arrow arr
      | O.T_module_accessor ma ->
        let ma = module_access self ma in
        return @@ T_module_accessor ma
      | O.T_singleton x -> return @@ I.T_singleton x
      | O.T_abstraction x ->
        let type_ = self x.type_ in
        return @@ I.T_abstraction { x with type_ }
      | O.T_for_all x ->
        let type_ = self x.type_ in
        return @@ I.T_for_all { x with type_ }

let rec decompile_expression : O.expression -> I.expression =
  fun e ->
  let self = decompile_expression in
  let self_type = decompile_type_expression in
  let return expr = I.make_e ~loc:e.location expr in
  match e.sugar with
    Some e -> e
  | None ->
    match e.expression_content with
      O.E_literal lit -> return @@ I.E_literal (lit)
    | O.E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:self arguments in
      return @@ I.E_constant {cons_name = cons_name;arguments}
    | O.E_variable name -> return @@ I.E_variable (cast_var name)
    | O.E_application app ->
      let app = application self app in
      return @@ I.E_application app
    | O.E_lambda lamb ->
      let lamb = lambda self self_type lamb in
      return @@ I.E_lambda lamb
    | O.E_recursive recs ->
      let recs = recursive self self_type recs in
      return @@ I.E_recursive recs
    | O.E_let_in {let_binder = {var; ascr};attr={inline=false};rhs=expr1;let_result=expr2}
      when Var.equal var.wrap_content (Var.of_name "_")
           && Stdlib.(=) ascr (Some (O.t_unit ())) ->
      let expr1 = self expr1 in
      let expr2 = self expr2 in
      return @@ I.E_sequence {expr1;expr2}
    | O.E_let_in {let_binder;attr;rhs;let_result} ->
      let let_binder = binder self_type let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      let attributes = if attr.inline then ["inline"] else [] in
      return @@ I.E_let_in {let_binder;mut=false;attributes;rhs;let_result}
    | O.E_type_in {type_binder; rhs; let_result} ->
      let rhs = self_type rhs in
      let let_result = self let_result in
      return @@ I.E_type_in {type_binder; rhs; let_result}
    | O.E_mod_in {module_binder;rhs;let_result} ->
      let rhs = decompile_module rhs in
      let let_result = self let_result in
      return @@ I.E_mod_in {module_binder;rhs;let_result}
    | O.E_mod_alias ma ->
      let ma = mod_alias self ma in
      return @@ I.E_mod_alias ma
    | O.E_raw_code rc ->
      let rc = raw_code self rc in
      return @@ I.E_raw_code rc
    | O.E_constructor const ->
      let const = constructor self const in
      return @@ I.E_constructor const
    | O.E_matching {matchee; cases} ->
      let matchee = self matchee in
      let aux :
        (O.expression, O.type_expression) O.match_case -> (I.expression, I.type_expression) I.match_case =
          fun {pattern ; body} ->
            let body = self body in
            let pattern = Stage_common.Helpers.map_pattern_t (binder self_type) pattern in
            I.{pattern ; body}
      in
      let cases = List.map ~f:aux cases in
      return @@ I.E_matching {matchee ; cases}
    | O.E_record record ->
      let record = O.LMap.to_kv_list_rev record in
      let record =
        List.map ~f:(fun (O.Label k,v) ->
          let v = self v in
          (I.Label k,v)
        ) record
      in
      return @@ I.E_record (I.LMap.of_list record)
    | O.E_record_accessor {record;path} ->
      let record = self record in
      let Label path  = path in
      return @@ I.E_accessor {record;path=[I.Access_record path]}
    | O.E_record_update {record;path;update} ->
      let record = self record in
      let update = self update in
      let Label path  = path in
      return @@ I.E_update {record;path=[I.Access_record path];update}
    | O.E_ascription {anno_expr; type_annotation} ->
      let anno_expr = self anno_expr in
      let type_annotation = decompile_type_expression type_annotation in
      return @@ I.E_ascription {anno_expr; type_annotation}
    | O.E_module_accessor ma ->
      let ma = module_access self ma in
      return @@ E_module_accessor ma

and decompile_lambda : _ O.lambda -> _ I.lambda =
  fun {binder=b;output_type;result}->
    let binder = binder decompile_type_expression b in
    let output_type = Option.map ~f:decompile_type_expression output_type in
    let result = decompile_expression result in
    I.{binder;output_type;result}

and decompile_declaration : O.declaration -> I.declaration =
  fun declaration ->
  let return (decl: I.declaration) = decl in
  match declaration with
  | O.Declaration_type {type_binder; type_expr; type_attr=_} ->
    let type_expr = decompile_type_expression type_expr in
    return @@ I.Declaration_type {type_binder; type_expr; type_attr=[]}
  | O.Declaration_constant {name; binder=b; attr={inline}; expr} ->
    let binder = binder decompile_type_expression b in
    let expr = decompile_expression expr in
    let attr = if inline then ["inline"] else [] in
    return @@ I.Declaration_constant {name; binder; attr; expr}
  | O.Declaration_module {module_binder;module_;module_attr=_} ->
    let module_ = decompile_module module_ in
    return @@ I.Declaration_module {module_binder;module_;module_attr=[]}
  | O.Module_alias ma ->
    let ma = module_alias ma in
    return @@ Module_alias ma

and decompile_module : O.module_ -> I.module_ = fun m ->
  List.map ~f:(Location.map decompile_declaration) m
