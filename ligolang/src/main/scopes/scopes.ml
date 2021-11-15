open Types
open Misc

module Formatter = Formatter

type sub_module = { type_env : tenv  ; bindings : bindings_map }

let scopes : with_types:bool -> options:Compiler_options.t -> Ast_core.module_ -> (def_map * scopes) = fun ~with_types ~options core_prg ->
  let make_v_def_from_core = make_v_def_from_core ~with_types  in
  let make_v_def_option_type = make_v_def_option_type ~with_types in

  let rec find_scopes' = fun (i,all_defs,env,scopes,lastloc) (bindings:bindings_map) (e : Ast_core.expression) ->
    match e.expression_content with
    | E_let_in { let_binder = {var ; ascr} ; rhs ; let_result } -> (
      let (i,all_defs,_, scopes) = find_scopes' (i,all_defs,env,scopes,e.location) bindings rhs in
      let def = make_v_def_option_type bindings var ascr var.location rhs.location in
      let (i,env) = add_shadowing_def (i,var.wrap_content) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_type_in { type_binder; rhs ; let_result } -> (
      let def = make_t_def (get_binder_name type_binder) e.location rhs in
      let (i,env) = add_shadowing_def (i,type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_mod_in { module_binder; rhs; let_result } -> (
      let (i,new_outer_def_map,_new_inner_def_map,scopes,_) = declaration ~options i rhs in
      let def = make_m_def module_binder e.location new_outer_def_map in
      let env = Def_map.add module_binder def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_mod_alias { alias; binders ; result } -> (
      let env_opt = Def_map.find_opt (fst binders) env in
      let aux def_opt binder =
        match def_opt with
        | Some Module m -> Def_map.find_opt binder m.content
        | _ -> None
      in
      let def = List.fold_left ~f:aux ~init:env_opt (snd binders) in
      let env = match def with 
        | Some def -> Def_map.add alias def env
        | None -> env
      in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_recursive { fun_name ; fun_type ; lambda = { result ; _ } } -> (
      let def = make_v_def_option_type bindings fun_name (Some fun_type) fun_name.location result.location in
      let (i,env) = add_shadowing_def (i,fun_name.wrap_content) def env in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_lambda { binder={var;ascr=input_type} ; output_type = _ ; result } -> (
      let def = make_v_def_option_type bindings var input_type var.location result.location in
      let (i,env) = add_shadowing_def (i,var.wrap_content) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_matching {matchee; cases} -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,matchee.location) bindings matchee in
      let aux = fun (i,all_defs,scopes) ({pattern;body}: (Ast_core.expression,_) Ast_core.match_case) ->
        let aux (i,env) (p: _ Ast_core.pattern) =
          match p.wrap_content with
          | Ast_core.P_var binder ->
            let proj_def = make_v_def_from_core bindings binder.var binder.var.location binder.var.location in
            add_shadowing_def (i,binder.var.wrap_content) proj_def env
          | _ -> (i,env)
        in
        let (i,env) = Stage_common.Helpers.fold_pattern aux (i,env) pattern in
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,body.location) bindings body in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left ~f:aux ~init:(i,all_defs,scopes) cases in
      (i,all_defs,env,scopes)
    )
    | E_record emap -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) ->
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) bindings exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left ~f:aux ~init:(i,all_defs,scopes) (Ast_core.LMap.to_list emap) in
      (i,all_defs,env,scopes)
    )
    | E_record_update { record ; update ; _ } -> (
      (*TODO: here record has a virtual location, check this out.. not normal *)
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,record.location) bindings record in
      find_scopes' (i,all_defs,env,scopes,update.location) bindings update
    )
    | E_constant { arguments ; _ } -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) ->
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) bindings exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left ~f:aux ~init:(i,all_defs,scopes) arguments in
      (i,all_defs,env,scopes)
    )
    | E_application { lamb ; args } -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,lamb.location) bindings lamb in
      find_scopes' (i,all_defs,env,scopes,args.location) bindings args
    )
    | E_ascription { anno_expr=e;_ } | E_record_accessor { record=e;_ } | E_constructor { element=e;_ } -> (
      find_scopes' (i,all_defs,env,scopes,e.location) bindings e
    )
    | E_module_accessor { module_name; element=e} ->
      let env_opt = Def_map.find_opt module_name env in
      let env = match env_opt with 
        | Some Module def -> def.content
        | _ -> env
      in
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,e.location) bindings e  in
      (i,all_defs,env,scopes)
    | E_variable x -> (
      let env = add_reference x env in
      let all_defs = merge_defs env all_defs in
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
    | E_literal _ | E_raw_code _ -> (
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
  and find_scopes (i,top_lvl_defs,scopes,loc) bindings  e =
    let (i,defs,_,scopes) = find_scopes' (i,top_lvl_defs,top_lvl_defs,scopes,loc) bindings e in
    (i,defs,scopes)

  and declaration ~options i core_prg =
    let test = options.test in
    let compile_declaration ~raise env decl () = Checking.type_declaration ~raise ~test ~protocol_version:options.protocol_version env decl in
    let aux = fun (i,top_def_map,inner_def_map,scopes,partials) (decl : Ast_core.declaration Location.wrap) ->
      let typed_prg =
        if with_types then Trace.to_option (compile_declaration partials.type_env decl ())
        else None
      in
      let partials = match typed_prg with
        | Some (type_env,decl') ->
          let bindings = extract_variable_types partials.bindings decl'.wrap_content in
          { type_env ; bindings }
        | None -> partials
      in
      match decl.wrap_content with
      | Declaration_constant { binder= { var ; ascr } ; expr ; _ } -> (
        let (i,new_inner_def_map,scopes) = find_scopes (i,top_def_map,scopes,decl.location) partials.bindings expr in
        let inner_def_map = merge_defs new_inner_def_map inner_def_map in
        let def = make_v_def_option_type partials.bindings var ascr var.location expr.location in
        let (i,top_def_map) = add_shadowing_def (i,var.wrap_content) def top_def_map in
        ( i, top_def_map, inner_def_map, scopes , partials )
      )
      | Declaration_type {type_binder; type_expr} -> (
        let def = make_t_def (get_binder_name type_binder) decl.location type_expr in
        let (i,top_def_map) = add_shadowing_def (i,type_binder) def top_def_map in
        ( i, top_def_map, inner_def_map, scopes, partials )
      )
      | Declaration_module {module_binder; module_} -> (
        let (i,new_outer_def_map,_new_inner_def_map,scopes,_) = declaration ~options i module_ in
        let def = make_m_def module_binder decl.location new_outer_def_map in
        let top_def_map = Def_map.add module_binder def top_def_map in
        ( i, top_def_map, inner_def_map, scopes, partials )
      )
      | Module_alias {alias; binders} -> (
        let env_opt = Def_map.find_opt (fst binders) top_def_map in
        let aux def_opt binder = match def_opt with
          | Some Module m -> Def_map.find_opt binder m.content
          | _ -> None
        in
        let def = List.fold_left ~f:aux ~init:env_opt (snd binders) in
        let top_def_map = match def with 
          | Some def -> Def_map.add alias def top_def_map
          | None -> top_def_map
        in
        ( i, top_def_map, inner_def_map, scopes, partials )
      )
    in
    let init = { type_env = options.init_env ; bindings = Bindings_map.empty } in
    List.fold_left ~f:aux ~init:(i, Def_map.empty, Def_map.empty, [], init) core_prg 
  in
  let (_,top_d,inner_d,s,_) = declaration ~options 0 core_prg in
  let d = Def_map.union merge_refs top_d inner_d in
  (d,s)
