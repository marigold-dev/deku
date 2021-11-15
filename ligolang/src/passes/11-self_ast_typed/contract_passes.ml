open Ast_typed.Types
open Trace

type contract_pass_data = {
  contract_type : Helpers.contract_type ;
  main_name : string ;
}

let annotation_or_label annot label = String.capitalize_ascii (Option.value ~default:label (Ast_typed.Helpers.remove_empty_annotation annot))

let check_entrypoint_annotation_format ~raise ep (exp: expression) =
  match String.split_on_char '%' ep with
    | [ "" ; ep'] ->
      let cap = String.capitalize_ascii ep' in
      if String.equal cap ep' then raise.raise @@ Errors.bad_format_entrypoint_ann ep exp.location
      else cap
    | _ -> raise.raise @@ Errors.bad_format_entrypoint_ann ep exp.location 


let self_typing ~raise : contract_pass_data -> expression -> bool * contract_pass_data * expression = fun dat e ->
  let bad_self_err () = Errors.bad_self_type
    e.type_expression
    {e.type_expression with
      type_content =
        T_constant {
          language=Stage_common.Backends.michelson;
          injection=Ligo_string.verbatim Stage_common.Constant.contract_name;
          parameters=[dat.contract_type.parameter]
        }
    }
    e.location
  in
  match e.expression_content , e.type_expression with
  | (E_constant {cons_name=C_SELF ; arguments=[entrypoint_exp]} , {type_content = T_constant {language=_;injection;parameters=[t]} ; _}) when String.equal (Ligo_string.extract injection) Stage_common.Constant.contract_name ->
    let entrypoint =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
      | _ -> raise.raise @@ Errors.entrypoint_annotation_not_literal entrypoint_exp.location
    in
    let entrypoint_t =
      match dat.contract_type.parameter.type_content with
      | (T_sum _ as t) when String.equal "Default" entrypoint -> {dat.contract_type.parameter with type_content = t}
      | T_sum cmap ->
        let content = LMap.to_kv_list cmap.content in
        let content = List.map ~f:(fun (Label entrypoint, {michelson_annotation;associated_type;_}) ->
                          (annotation_or_label michelson_annotation entrypoint, associated_type)) content in
        let associated_type = trace_option ~raise (Errors.unmatched_entrypoint entrypoint_exp.location) @@
          List.Assoc.find content ~equal:String.equal entrypoint
        in
        associated_type
      | t -> {dat.contract_type.parameter with type_content = t}
    in
    let () =
      trace_option ~raise (bad_self_err ()) @@
      Ast_typed.assert_type_expression_eq (entrypoint_t , t) in
    (true, dat, e)
  | _ -> (true,dat,e)

let entrypoint_typing ~raise : contract_pass_data -> expression -> bool * contract_pass_data * expression = fun dat e ->
  match e.expression_content with
  | E_constant {cons_name=C_CONTRACT_ENTRYPOINT_OPT|C_CONTRACT_ENTRYPOINT ; arguments=[entrypoint_exp;_]} ->
    let _ = match entrypoint_exp.expression_content with
     | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
     | _ -> raise.raise @@ Errors.entrypoint_annotation_not_literal entrypoint_exp.location
    in
    (true, dat, e)
  | _ -> (true,dat,e)

module VSet = Set.Make(struct
  type t = expression_ Var.t
  let compare = Var.compare
end)
(*
module VMap = Map.Make(struct
  type t = expression_ Var.t
  let compare = Var.compare
end)*)
type env = {env:env SMap.t;used_var:VSet.t}
let rec pp_env ppf env = 
  Format.fprintf ppf "{env: %a;used_var: %a}"
    (PP_helpers.list_sep_d (fun ppf (k,v) -> Format.fprintf ppf "(%s,%a)" k pp_env v)) (SMap.to_kv_list env.env)
    (PP_helpers.list_sep_d Var.pp) (VSet.elements env.used_var)

let remove_unused ~raise : string -> module_fully_typed -> module_fully_typed = fun main_name prg ->
  let Module_Fully_Typed module' = prg in
  (* Process declaration in reverse order *)
  let prg_decls = List.rev module' in
  let aux = function
      {Location.wrap_content = Declaration_constant {name = Some name;  _}; _} -> not (String.equal name main_name)
    | _ -> true in
  (* Remove the definition after the main entry_point (can't be relevant), mostly remove the test *)
  let _, prg_decls = List.split_while prg_decls ~f:aux in
  let main_decl, prg_decls = trace_option ~raise (Errors.corner_case "Entrypoint not found") @@ List.uncons prg_decls in
  let main_dc = trace_option ~raise (Errors.corner_case "Entrypoint not found") @@ match main_decl with
      {Location.wrap_content = Declaration_constant dc; _} -> Some dc
    | _ -> None in
  (* Detect and remove unesed declaration *)
  let rec merge_env {env=x1;used_var=y1} {env=x2;used_var=y2} =
    let aux _ a b = Some (merge_env a b) in
      {env = SMap.union aux x1 x2; used_var = VSet.union y1 y2}
  in
  let unions = fun l ->
    List.fold l ~init:{env=SMap.empty;used_var=VSet.empty} ~f:merge_env
  in
  let rec get_fv expr =
    let self = get_fv in
    let return env expression_content = env, {expr with expression_content} in
    match expr.expression_content with
    | E_variable v ->
      return {env=SMap.empty ;used_var=VSet.singleton v.wrap_content} @@ E_variable v
    | E_literal _ | E_raw_code _ as ec ->
      return {env=SMap.empty ;used_var=VSet.empty} @@ ec
    | E_constant {cons_name;arguments} ->
      let env_lst,arguments = List.unzip @@ List.map ~f:self arguments in
      return (unions @@ env_lst) @@ E_constant {cons_name;arguments}
    | E_application {lamb; args} ->
      let env_l,lamb = self lamb in
      let env_a,args = self args in
      return (merge_env env_l env_a) @@ E_application {lamb;args}
    | E_type_inst {forall;type_} ->
      let env,forall = self forall in
      return env @@ E_type_inst {forall;type_}
    | E_lambda {binder ; result} ->
      let {env;used_var},result = self result in
      return {env;used_var=VSet.remove binder.wrap_content @@ used_var} @@ E_lambda {binder;result}
    | E_recursive {fun_name; lambda = {binder; result};fun_type} ->
      let {env;used_var},result = self result in
      return {env;used_var=VSet.remove fun_name.wrap_content @@ VSet.remove binder.wrap_content @@ used_var} @@
      E_recursive {fun_name; lambda = {binder; result};fun_type}
    | E_constructor {constructor;element} ->
      let env,element = self element in
      return env @@ E_constructor {constructor;element}
    | E_matching {matchee; cases} ->
      let env,matchee = self matchee in
      let env_c,cases = get_fv_cases cases in
      return (merge_env env env_c) @@ E_matching{matchee;cases}
    | E_record m ->
      let res = LMap.map self m in
      let keys,env_exp = List.unzip @@ LMap.to_kv_list res in
      let env,exp = List.unzip env_exp in
      let m = LMap.of_list @@ List.zip_exn keys exp in
      return (unions env) @@ E_record m
    | E_record_update {record;path;update} ->
      let env_r,record = self record in
      let env_u,update = self update in
      return (merge_env env_r env_u) @@ E_record_update {record;path;update}
    | E_record_accessor {record;path} ->
      let env, record = self record in
      return env @@ E_record_accessor {record;path}
    | E_let_in { let_binder ; rhs ; let_result ; attr} ->
      let env,let_result = (self let_result) in
      let env = {env with used_var=VSet.remove let_binder.wrap_content env.used_var} in
      let env', rhs = self rhs in
      return (merge_env env env') @@ E_let_in {let_binder; rhs; let_result; attr}
    | E_type_in {type_binder;rhs;let_result} ->
      let env,let_result = self let_result in
      return env @@ E_type_in {type_binder;rhs;let_result}
    | E_mod_in { module_binder; rhs ; let_result } ->
      let env,let_result = (self let_result) in
      (match SMap.find_opt module_binder env.env with
        Some (env') ->
          let env = {env with env = SMap.remove module_binder env.env} in
          let Module_Fully_Typed rhs = rhs in
          let env',rhs = get_fv_module env'[] @@ List.rev rhs in
          let rhs = Module_Fully_Typed rhs in
          return (merge_env env env') @@ E_mod_in {module_binder; rhs; let_result}
      | None ->
        env,let_result
      )
    | E_mod_alias { alias ; binders ; result } ->
      let env, result = self result in
      return env @@ E_mod_alias {alias;binders;result}
    | E_module_accessor { module_name; element } ->
      let env,element = self element in
      return ({env=SMap.singleton module_name env;used_var=VSet.empty}) @@ E_module_accessor {module_name;element}
  and get_fv_cases : matching_expr -> env * matching_expr = fun m ->
    match m with
    | Match_variant {cases;tv} ->
      let aux {constructor; pattern ; body} =
        let env,body= get_fv body in
        {env with used_var=VSet.remove pattern.wrap_content @@ env.used_var},{constructor;pattern;body} in
      let envs,cases = List.unzip @@  List.map ~f:aux cases in
      (unions envs), Match_variant {cases;tv}
    | Match_record {fields; body; tv} ->
      let pattern = LMap.values fields |> List.map ~f:fst |> List.map ~f:Location.unwrap in
      let env,body = get_fv body in
      {env with used_var=List.fold_right pattern ~f:VSet.remove ~init:env.used_var}, Match_record {fields;body;tv}

  and _add_binders (name, name_lst) env =
    let env' = match name_lst with 
      [] -> {env=SMap.empty;used_var=VSet.empty}
    | hd :: tl -> _add_binders (hd,tl) env
    in
    let env': env = {env=SMap.singleton name env';used_var=VSet.empty} in
    merge_env env' env

  and get_fv_module env acc = function
    | [] -> env, acc
    | {Location.wrap_content = Declaration_constant {name;binder; expr;attr}; _} as hd :: tl ->
       let binder' = binder.wrap_content in
       if VSet.mem binder' env.used_var then
        let env = {env with used_var = VSet.remove binder' env.used_var} in
        let env',expr = get_fv expr in
        let env = merge_env env @@ env' in
        get_fv_module env ({hd with wrap_content = Declaration_constant {name;binder;expr;attr}} :: acc) tl
       else
         get_fv_module env acc tl
    | {Location.wrap_content = Declaration_module {module_binder; module_;module_attr}; _} as hd :: tl ->
       (match SMap.find_opt module_binder env.env with
        Some (env') ->
          let env = {env with env = SMap.remove module_binder env.env} in
          let Module_Fully_Typed module_ = module_ in
          let new_env,module_ = get_fv_module env' [] @@ List.rev module_ in
          let env = merge_env env new_env in
          let module_ = Module_Fully_Typed module_ in
          get_fv_module env ({hd with wrap_content=Declaration_module{module_binder;module_;module_attr}} :: acc) tl
      | None ->
          get_fv_module env acc tl
          )
    | {Location.wrap_content = Module_alias {alias = name; binders}; _} as hd :: tl ->
      let rec push_env (name,name_lst) toto =
        match name_lst with 
          [] -> {env=SMap.singleton name toto;used_var=VSet.empty}
        | hd::tl -> {env=SMap.singleton name @@ push_env (hd,tl) toto;used_var=VSet.empty}
      in
       if SMap.mem name env.env then
         let toto = SMap.find name env.env in
         let env' = {env with env = SMap.remove name env.env} in
         let env = merge_env env' @@ push_env binders toto in
         get_fv_module env (hd :: acc) tl
       else
         get_fv_module env acc tl
    | hd :: tl ->
       get_fv_module env (hd :: acc) tl in
       
  let env,main_expr = get_fv main_dc.expr in
  let main_dc = {main_dc with expr = main_expr} in
  let main_decl = {main_decl with wrap_content = Declaration_constant main_dc} in
  let _,module_ = get_fv_module env [main_decl] prg_decls in
  Module_Fully_Typed module_

