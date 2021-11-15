open Core

let pair_map = fun f (x , y) -> (f x , f y)

module Substitution = struct
  module Pattern = struct

    module T = Ast_core
    (* module TSMap = Trace.TMap(String) *)

    type substs = variable:type_variable -> T.type_content option (* this string is a type_name or type_variable I think *)
    let mk_substs ~v ~expr = (v , expr)

    type ('a, 'err) w = substs:substs -> 'a -> 'a

    let rec rec_yes = true
    and s_environment_element_definition ~substs = function
      | T.ED_binder -> T.ED_binder
      | T.ED_declaration T.{expression ; free_variables} ->
        let expression = s_expression ~substs expression in
        let free_variables = List.map ~f:(s_variable ~substs) free_variables in
        T.ED_declaration {expression ; free_variables}
    and s_expr_environment : (T.expression_environment,_) w = fun ~substs env ->
      List.map ~f:(fun T.{expr_var=variable ; env_elt={ type_value; definition }} ->
          let type_value = s_type_expression ~substs type_value in
          let definition = s_environment_element_definition ~substs definition in
          T.{expr_var=variable ; env_elt={ type_value; definition }}) env
    and s_type_environment : (T.type_environment,_) w = fun ~substs tenv ->
      List.map ~f:(fun T.{type_variable ; type_} ->
        let type_ = s_type_expression ~substs type_ in
        T.{type_variable ; type_}) tenv
    and s_module_environment: (T.module_environment,_) w = fun ~substs menv ->
      List.map ~f:(fun T.{module_variable ; module_} ->
        let module_ = s_environment ~substs module_ in
        T.{module_variable;module_}) menv
    and s_environment : (T.environment,_) w = fun ~substs T.{expression_environment ; type_environment ; module_environment} ->
      let expression_environment = s_expr_environment ~substs expression_environment in
      let type_environment = s_type_environment ~substs type_environment in
      let module_environment = s_module_environment ~substs module_environment in
      T.{ expression_environment ; type_environment ; module_environment}

    and s_variable : (T.expression_variable,_) w = fun ~substs var ->
      let () = ignore @@ substs in
      var

    and s_type_variable : (T.type_variable,_) w = fun ~substs var ->
      let () = ignore @@ substs in
      var

    and s_binder : (_ T.binder,_) w = fun ~substs {var;ascr} ->
      let var = s_variable ~substs var in
      let ascr = Option.map ~f:(s_type_expression ~substs) ascr in
      T.{var;ascr;attributes=Stage_common.Helpers.empty_attribute}

    and s_label : (T.label,_) w = fun ~substs l ->
      let () = ignore @@ substs in
      l

    and s_build_in : (T.constant',_) w = fun ~substs b ->
      let () = ignore @@ substs in
      b

    and s_constructor : (T.label,_) w = fun ~substs c ->
      let () = ignore @@ substs in
      c

    and s_rows : (T.rows,_) w = fun ~substs rows ->
      let aux T.{ associated_type; michelson_annotation ; decl_pos } =
        let associated_type = s_type_expression ~substs associated_type in
        T.{ associated_type; michelson_annotation; decl_pos } in
      let fields = T.LMap.map aux rows.fields in
      { rows with fields }

    and s_type_content : (T.type_content,_) w = fun ~substs -> function
        | T.T_sum rows ->
          let rows = s_rows ~substs rows in
          T.T_sum rows
        | T.T_record rows ->
          let rows = s_rows ~substs rows in
          T.T_record rows
        | T.T_variable variable ->
           begin
             match substs ~variable with
             | Some expr -> s_type_content ~substs expr (* TODO: is it the right thing to recursively examine this? We mustn't go into an infinite loop. *)
             | None -> T.T_variable variable
           end
        | T.T_arrow { type1; type2 } ->
          let type1 = s_type_expression ~substs type1 in
          let type2 = s_type_expression ~substs type2 in
          T.T_arrow { type1; type2 }
        | T.T_app {type_operator;arguments} ->
          let arguments = List.map ~f:(s_type_expression ~substs) arguments in
          T.T_app {type_operator;arguments}
        | T.T_module_accessor { module_name; element } ->
          let element = s_type_expression ~substs element in
          T.T_module_accessor { module_name; element }
        | T.T_singleton x -> T.T_singleton x
        | T.T_abstraction x ->
          let type_ = s_type_expression ~substs x.type_ in
          T.T_abstraction {x with type_}
        | T.T_for_all x ->
          let type_ = s_type_expression ~substs x.type_ in
          T.T_for_all {x with type_}

    and s_type_expression : (T.type_expression,_) w = fun ~substs { type_content; location; sugar } ->
      let type_content = s_type_content ~substs type_content in
      T.{ type_content; location; sugar}
    and s_literal : (T.literal,_) w = fun ~substs -> function
      | T.Literal_unit ->
        let () = ignore @@ substs in
        T.Literal_unit
      | (T.Literal_int _ as x)
      | (T.Literal_nat _ as x)
      | (T.Literal_timestamp _ as x)
      | (T.Literal_mutez _ as x)
      | (T.Literal_string _ as x)
      | (T.Literal_bytes _ as x)
      | (T.Literal_address _ as x)
      | (T.Literal_signature _ as x)
      | (T.Literal_key _ as x)
      | (T.Literal_key_hash _ as x)
      | (T.Literal_chain_id _ as x)
      | (T.Literal_operation _ as x) ->
        x
    and s_matching_expr : (_ T.match_case list,_) w = fun ~(substs : substs) -> 
      fun x ->
        List.map ~f:
          (fun (x: _ T.match_case) -> let body = s_expression ~substs x.body in { x with body })
          x
    and s_accessor  : (_ T.record_accessor,_) w = fun ~substs {record;path} ->
      let record = s_expression ~substs record in
      ({record;path} : _ T.record_accessor)


    and s_expression_content : (T.expression_content,_) w = fun ~(substs : substs) -> function
      | T.E_literal         x ->
        let x = s_literal ~substs x in
        T.E_literal x
      | T.E_constant   {cons_name;arguments} ->
        let cons_name = s_build_in ~substs cons_name in
        let arguments = List.map ~f:(s_expression ~substs) arguments in
        T.E_constant {cons_name;arguments}
      | T.E_variable        tv ->
        let tv = s_variable ~substs tv in
        T.E_variable tv
      | T.E_application {lamb;args} ->
        let lamb = s_expression ~substs lamb in
        let args = s_expression ~substs args in
        T.E_application {lamb;args}
      | T.E_lambda          { binder; output_type; result } ->
        let binder = s_binder ~substs binder in
        let output_type = Option.map ~f:(s_type_expression ~substs) output_type in
        let result = s_expression ~substs result in
        T.E_lambda { binder; output_type; result }
      | T.E_let_in          { let_binder; rhs; let_result; attr} ->
        let let_binder = s_binder ~substs let_binder in
        let rhs = s_expression ~substs rhs in
        let let_result = s_expression ~substs let_result in
        T.E_let_in { let_binder; rhs; let_result; attr}
      | T.E_type_in          { type_binder; rhs; let_result} ->
        let type_binder = s_type_variable ~substs type_binder in
        let rhs = s_type_expression ~substs rhs in
        let let_result = s_expression ~substs let_result in
        T.E_type_in { type_binder; rhs; let_result}
      | T.E_mod_in          { module_binder; rhs; let_result} ->
        let rhs = s_module' ~substs rhs in
        let let_result = s_expression ~substs let_result in
        T.E_mod_in { module_binder; rhs; let_result}
      | T.E_mod_alias          { alias; binders; result} ->
        let result  = s_expression ~substs result in
        T.E_mod_alias { alias; binders; result}
      | T.E_raw_code {language; code} ->
        let code = s_expression ~substs code in
        T.E_raw_code {language; code}
      | T.E_recursive { fun_name; fun_type; lambda} ->
        let fun_name = s_variable ~substs fun_name in
        let fun_type = s_type_expression ~substs fun_type in
        let sec = s_expression_content ~substs (T.E_lambda lambda) in
        let lambda = match sec with E_lambda l -> l | _ -> failwith "impossible case" in
        T.E_recursive { fun_name; fun_type; lambda}
      | T.E_constructor  {constructor;element} ->
        let constructor = s_constructor ~substs constructor in
        let element = s_expression ~substs element in
        T.E_constructor {constructor;element}
      | T.E_record          aemap ->
        let aemap = List.map ~f:(fun (key,val_) ->
          let val_ = s_expression ~substs val_ in
          (key , val_)) @@ T.LMap.to_kv_list aemap in
        let aemap = T.LMap.of_list aemap in
        T.E_record aemap
      | T.E_record_accessor {record=e;path} ->
        let record = s_expression ~substs e in
        let path = s_label ~substs path in
        T.E_record_accessor {record;path}
      | T.E_record_update {record;path;update}->
        let record = s_expression ~substs record in
        let update = s_expression ~substs update in
        T.E_record_update {record;path;update}
      | T.E_matching   {matchee;cases} ->
        let matchee = s_expression ~substs matchee in
        let cases = s_matching_expr ~substs cases in
        T.E_matching {matchee;cases}
      | T.E_module_accessor { module_name; element } ->
        let element = s_expression ~substs element in
        T.E_module_accessor { module_name; element }
      | T.E_ascription {anno_expr; type_annotation} ->
        let anno_expr = s_expression ~substs anno_expr in
        let type_annotation = s_type_expression ~substs type_annotation in
        T.E_ascription {anno_expr; type_annotation}

    and s_expression : (T.expression,_) w = fun ~(substs:substs) { expression_content; sugar; location } ->
      let expression_content = s_expression_content ~substs expression_content in
      let location = location in
      T.{ expression_content;sugar; location }

    and s_declaration : (T.declaration,_) w =
    let return (d : T.declaration) = d in
    fun ~substs ->
      function
      | T.Declaration_constant {name ; binder ; expr ; attr} ->
        let binder = s_binder ~substs binder in
        let expr = s_expression ~substs expr in
        return @@ Declaration_constant {name; binder; expr; attr}
      | T.Declaration_type t -> return @@ Declaration_type t
      | T.Declaration_module {module_binder;module_;module_attr} ->
        let module_       = s_module' ~substs module_ in
        return @@ Declaration_module {module_binder;module_;module_attr}
      | T.Module_alias {alias;binders} ->
        return @@ Module_alias {alias; binders}

    and s_declaration_wrap : (T.declaration Location.wrap,_) w = fun ~substs d ->
      Location.map (s_declaration ~substs) d

    (* Replace the type variable ~v with ~expr everywhere within the
       module ~p. TODO: issues with scoping/shadowing. *)
    and s_module : (T.module_with_unification_vars,_) w = fun ~substs (T.Module_With_Unification_Vars p) ->
      let p = List.map ~f:(s_declaration_wrap ~substs) p in
      T.Module_With_Unification_Vars p

    and s_module' : (T.module_,_) w = fun ~substs (p) ->
      let p = List.map ~f:(s_declaration_wrap ~substs) p in
      p

    (*
       Computes `P[v := expr]`.
    *)
    and type_value : tv:type_value -> substs:type_variable * type_value -> type_value = fun ~tv ~substs ->
      let open T.Reasons in
      let self tv = type_value ~tv ~substs in
      let (v, expr) = substs in
      match tv.wrap_content with
      | P_variable v' when Var.equal v' v -> expr
      | P_variable _ -> tv
      | P_constant {p_ctor_tag=x ; p_ctor_args=lst} -> (
          let lst' = List.map ~f:self lst in
          wrap (Todo "1") @@ T.P_constant {p_ctor_tag=x ; p_ctor_args=lst'}
        )
      | P_apply { tf; targ } -> (
          T.Reasons.(wrap (Todo "2") @@ T.P_apply { tf = self tf ; targ = self targ})
        )
      | P_row {p_row_tag; p_row_args} ->
        let p_row_args = T.LMap.map (fun ({associated_value;michelson_annotation;decl_pos}: T.row_value ) -> ({associated_value=self associated_value;michelson_annotation;decl_pos} : T.row_value)) p_row_args in
        wrap (Todo "3") @@ T.P_row {p_row_tag ; p_row_args}
      | P_forall p -> (
          let aux c = constraint_ ~c ~substs in
          let constraints = List.map ~f:aux p.constraints in
          if (Var.equal p.binder v) then (
            (* The variable v is shadowed by the forall's binder, so
               we don't substitute inside the body. This should be
               handled in a more elegant manner once we have a proper
               environment and scopes. *)
            wrap (Todo "4") @@ T.P_forall { p with constraints }
          ) else (
            (* The variable v is still visible within the forall, so
               substitute also within the body *)
            let body = self p.body in
            wrap (Todo "5") @@ T.P_forall { p with constraints ; body }
          )
        )
      | P_abs _ -> failwith "P_abs : unimplemented"
      | P_constraint _ -> failwith "P_constraint : unimplemented"

    and constraint_ ~c:{c;reason} ~substs =
      {c = constraint__ ~c ~substs;reason}

    and constraint__ ~c ~substs =
      match c with
      | C_equation { aval; bval } -> (
        let aux tv = type_value ~tv ~substs in
          C_equation { aval = aux aval ; bval = aux bval }
        )
      | C_typeclass { tc_bound; tc_constraints; tc_args; original_id; typeclass=tc } -> (
          let tc_args = List.map ~f:(fun tv -> type_value ~tv ~substs) tc_args in
          let tc_constraints = List.map ~f:(fun c -> constraint_ ~c ~substs) tc_constraints in
          let tc = typeclass ~tc ~substs in
          C_typeclass {tc_bound; tc_constraints; tc_args ; original_id; typeclass=tc}
        )
      | C_access_label { c_access_label_record_type; accessor; c_access_label_tvar } -> (
          let c_access_label_record_type = type_value ~tv:c_access_label_record_type ~substs in
          C_access_label {c_access_label_record_type ; accessor ; c_access_label_tvar}
        )
      | c -> c

    and typeclass ~tc ~substs =
      List.map ~f:(List.map ~f:(fun tv -> type_value ~tv ~substs)) tc

    (* let module = s_module *)

    (* Performs beta-reduction at the root of the type *)
    let eval_beta_root ~(tv : type_value) =
      match tv.wrap_content with
      | P_apply {tf = { location = _ ; wrap_content = P_forall { binder; constraints; body } }; targ} ->
        let constraints = List.map ~f:(fun c -> constraint_ ~c ~substs:(mk_substs ~v:binder ~expr:targ)) constraints in
        (* TODO: indicate in the result's tsrc that it was obtained via beta-reduction of the original type *)
        (type_value ~tv:body ~substs:(mk_substs ~v:binder ~expr:targ) , constraints)
      | _ -> (tv , [])
  end

end
