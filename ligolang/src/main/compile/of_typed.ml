open Trace
open Ast_typed
open Spilling
open Main_errors

module SMap = Map.Make(String)

let compile_with_modules ~raise : Ast_typed.module_fully_typed -> Mini_c.program = fun p ->
  trace ~raise spilling_tracer @@ compile_module p

let compile ~raise : Ast_typed.module_fully_typed -> Mini_c.program = fun p ->
  let mini_c = compile_with_modules ~raise p in
  mini_c

let compile_expression ~raise : expression -> Mini_c.expression = fun e ->
  trace ~raise spilling_tracer @@ compile_expression e

let compile_type ~raise : type_expression -> Mini_c.type_expression = fun e ->
  trace ~raise spilling_tracer @@ compile_type e

let assert_equal_contract_type ~raise : Simple_utils.Runned_result.check_type -> string -> Ast_typed.module_fully_typed -> Ast_typed.expression -> unit  =
    fun c entry contract param ->
  let entry_point = trace_option ~raise main_entrypoint_not_found (Ast_typed.get_entry contract entry) in
  trace ~raise (check_typed_arguments_tracer c) (
    fun ~raise ->
    match entry_point.type_expression.type_content with
    | T_arrow {type1=args} -> (
        match args.type_content with
        | T_record m when LMap.cardinal m.content = 2 -> (
          let {associated_type=param_exp;_} = LMap.find (Label "0") m.content in
          let {associated_type=storage_exp;_} = LMap.find (Label "1") m.content in
            match c with
            | Check_parameter -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (param_exp, param.type_expression)
            | Check_storage   -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (storage_exp, param.type_expression)
        )
        | _ -> raise.raise @@ main_entrypoint_not_a_function )
    | _ -> raise.raise @@ main_entrypoint_not_a_function
  )

let rec get_views : Ast_typed.environment -> (string * location) list = fun e ->
  let f : (string * location) list -> environment_binding -> (string * location) list =
    fun acc {expr_var ; env_elt } ->
      match env_elt.definition with
      | ED_declaration { attr ; _ } when attr.view -> (Var.to_name expr_var.wrap_content, expr_var.location)::acc
      | _ -> acc
  in 
  let x = List.fold e.expression_environment ~init:[] ~f in
  let y = List.fold e.module_environment ~init:[] ~f:(fun acc x -> List.append acc (get_views x.module_)) in
  List.append x y

let decompile_env e = Checking.decompile_env e
let list_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_constant {binder;_} -> (Var.to_name binder.wrap_content)::prev
      | _ -> prev)
    ~init:[] m

let list_type_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_type {type_binder;_} -> (Var.to_name type_binder)::prev
      | _ -> prev)
    ~init:[] m

let list_mod_declarations (m : Ast_typed.module') : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_typed.declaration) with
      | Declaration_module {module_binder;_} -> (module_binder)::prev
      | Module_alias {alias;_} -> (alias)::prev
      | _ -> prev)
    ~init:[] m
