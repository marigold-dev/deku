open Trace
module Errors = Errors
open Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

type protocol_version = Environment.Protocols.t
module Environment = O.Environment

type environment = Environment.t

let cast_var (orig: 'a Var.t Location.wrap) = { orig with wrap_content = Var.todo_cast orig.wrap_content}
let assert_type_expression_eq = Helpers.assert_type_expression_eq

(* The `table` represents the substitutions that have been inferred.
   For example, if matching `a -> b -> a` with `int -> bool -> int`,
   it should have information such as `[a ↦ int; b ↦ bool]`. *)
module TMap = Map.Make(struct type t = O.type_variable let compare x y = Var.compare x y end)

let rec infer_type_application ~raise ~loc ?(default_error = fun loc t t' -> assert_equal loc t t') table (type_matched : O.type_expression) (type_ : O.type_expression) =
  let open O in
  let self = infer_type_application ~raise ~loc ~default_error in
  let default_error = default_error loc type_matched type_ in
  match type_matched.type_content, type_.type_content with
  | T_variable v, _ -> (
     match TMap.find_opt v table with
     | Some t -> trace_option ~raise (not_matching loc t type_) (assert_type_expression_eq (type_, t));
                 table
     | None -> TMap.add v type_ table)
  | T_arrow {type1;type2}, T_arrow {type1=type1_;type2=type2_} ->
     let table = self table type1 type1_ in
     let table = self table type2 type2_ in
     table
  | T_constant {language;injection;parameters}, T_constant {language=language';injection=injection';parameters=parameters'} ->
     if String.equal language language' && String.equal (Ligo_string.extract injection) (Ligo_string.extract injection') && Int.equal (List.length parameters) (List.length parameters') then
       let table = List.fold_right (List.zip_exn parameters parameters') ~f:(fun (t, t') table ->
                       self table t t') ~init:table in
       table
     else
       raise.raise default_error
  | T_record {content; layout}, T_record {content=content'; layout=layout'} ->
     let content_kv = O.LMap.to_kv_list content in
     let content'_kv = O.LMap.to_kv_list content' in
     if layout_eq layout layout' &&
          List.equal equal_label (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, {associated_type;michelson_annotation;decl_pos}), (_, {associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=decl_pos'})) table =
         if Int.equal decl_pos decl_pos' && Option.equal String.equal michelson_annotation michelson_annotation' then
           self table associated_type associated_type'
         else
           raise.raise default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.raise default_error
  | T_sum {content; layout}, T_sum {content=content'; layout=layout'} ->
     let content_kv = O.LMap.to_kv_list content in
     let content'_kv = O.LMap.to_kv_list content' in
     if layout_eq layout layout' &&
          List.equal equal_label (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, {associated_type;michelson_annotation;decl_pos}), (_, {associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=decl_pos'})) table =
         if Int.equal decl_pos decl_pos' && Option.equal String.equal michelson_annotation michelson_annotation' then
           self table associated_type associated_type'
         else
           raise.raise default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.raise default_error
  | (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _),
    (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _)
    -> raise.raise default_error

(* This function does some inference for applications: it takes a type
   `typed_matched` of the form `t1 -> ... -> tn -> t`, a list of types
   `args` of the form `[t'1;...;t'n]` (representing types on which the
   function is applied) and possibly a final type `tv_opt` of the form
   `t'` (representing an annotation for the final result).
   It will try to infer a table s.t. when substituting variables in
   `t1 -> ... -> tn -> t`, we get `t'1 -> ... > t'n -> t'`. It works
   by matching iteratively on each type: `t1` with `t'1`, ..., `tn`
   with `t'n`, and finally `t` with `t'`. *)
let infer_type_applications ~raise ~loc type_matched args tv_opt =
  let table, type_matched = List.fold_left args ~init:(TMap.empty, type_matched) ~f:(fun ((table, type_matched) : _ TMap.t * O.type_expression) matched ->
                  match type_matched.type_content with
                  | T_arrow { type1 ; type2 } ->
                     infer_type_application ~raise ~loc table type1 matched, type2
                  | (T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _) ->
                     table, type_matched) in
  match tv_opt with
  | Some t -> infer_type_application ~raise ~loc ~default_error:(fun loc t t' -> assert_equal loc t' t) table type_matched t
  | None -> table

(* This wraps a `∀ a . (∀ b . (∀ c . some_type))` with type instantiations,
   e.g. given the table `[a ↦ int; b ↦ string; c ↦ bool` it will return
   `(((∀ a . (∀ b . (∀ c . some_type))) @@ int) @@ string) @@ bool` *)
let build_type_insts ~raise ~loc (forall : O.expression) table bound_variables =
  let bound_variables = List.rev bound_variables in
  let rec build_type_insts (forall : O.expression) = function
    | [] -> forall
    | av :: avs' ->
       let ty_binder, t = trace_option ~raise (corner_case "Expected a for all type quantifier") @@ O.get_t_for_all forall.type_expression in
       assert (Var.equal (Location.unwrap ty_binder) av);
       let type_ = trace_option ~raise (Errors.not_annotated loc) @@ TMap.find_opt av table in
       build_type_insts (make_e (E_type_inst {forall ; type_ }) (Ast_typed.Helpers.subst_type av type_ t)) avs' in
  build_type_insts forall bound_variables

let rec type_module ~raise ~test ~init_env ~protocol_version (p:I.module_) : environment * O.module_fully_typed =
  let aux (e, acc:(environment * O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let (e', d') = type_declaration ~raise ~test ~protocol_version e d in
    (e', d' :: acc)
  in
  let (e, lst) =
      List.fold ~f:aux ~init:(init_env, []) p in
  let p = List.rev lst in
  (* the typer currently in use doesn't use unification variables, so there is no need to check for their absence. *)
  let p = O.Module_Fully_Typed p in
  (e,p)


and type_declaration : raise: typer_error raise -> protocol_version:protocol_version -> test: bool -> environment -> I.declaration Location.wrap -> environment * O.declaration Location.wrap =
fun ~raise ~protocol_version ~test env d ->
let return ?(loc = d.location) e (d : O.declaration) = e,Location.wrap ~loc d in
match Location.unwrap d with
  | Declaration_type {type_binder ; _} when Ast_core.Helpers.is_generalizable_variable type_binder ->
    raise.raise (wrong_generalizable d.location type_binder)
  | Declaration_type {type_binder ; type_expr; type_attr={public}} -> (
    let type_binder = Var.todo_cast type_binder in
    let tv = evaluate_type ~raise ~other_module:false env type_expr in
    let env' = Environment.add_type ~public type_binder tv env in
    return env' @@ Declaration_type { type_binder ; type_expr = tv; type_attr={public} }
  )
  | Declaration_constant {name ; binder = { ascr = None ; var } ; attr  ; expr} -> (
    let expr =
      trace ~raise (constant_declaration_error_tracer var expr None) @@
      type_expression' ~test ~protocol_version env expr in
    let binder : O.expression_variable = cast_var var in
    let post_env = Environment.add_ez_declaration ~public:attr.public binder expr attr env in
    return post_env @@ Declaration_constant { name ; binder ; expr ; attr }
  )
  | Declaration_constant {name ; binder = { ascr = Some tv ; var } ; attr ; expr } ->
    let type_env = List.map env.type_environment ~f:(fun { type_variable ; type_ = _ } -> type_variable) in
    let tv = Ast_core.Helpers.generalize_free_vars type_env tv in
    let av, tv = Ast_core.Helpers.destruct_for_alls tv in
    let pre_env = env in
    let env = List.fold_right av ~f:(fun v env -> Environment.add_type_var v () env) ~init:env in
    let tv = evaluate_type ~raise env tv in
    let expr =
      trace ~raise (constant_declaration_error_tracer var expr (Some tv)) @@
      type_expression' ~test ~protocol_version ~tv_opt:tv env expr in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all (Location.wrap abs_var) () (aux t abs_vars) in
    let type_expression = aux expr.type_expression (List.rev av) in
    let expr = { expr with type_expression } in
    let binder : O.expression_variable = cast_var var in
    let post_env = Environment.add_ez_declaration ~public:attr.public binder expr attr pre_env in
    return post_env @@ Declaration_constant { name ; binder ; expr ; attr }
  | Declaration_module {module_binder;module_; module_attr = {public}} -> (
    let e,module_ = type_module ~raise ~test ~protocol_version ~init_env:env module_ in
    let post_env = Environment.add_module ~public module_binder e env in
    return post_env @@ Declaration_module { module_binder; module_; module_attr = {public}}
  )
  | Module_alias {alias;binders} -> (
    let aux other_module env binder =
      trace_option ~raise (unbound_module_variable env binder d.location)
      @@ Environment.get_module_opt ?other_module:(Some other_module) binder env 
    in
    let (hd, tl) = binders in
    let e = List.fold_left ~f:(aux true) ~init:(aux false env hd) tl in
    let post_env = Environment.add_module ~public:true alias e env in
    return post_env @@ Module_alias { alias; binders}
  )

and evaluate_otype ~raise ?other_module (e:environment) (t:O.type_expression) : O.type_expression =
  (* NOTE: this is similar to evaluate_type, but just look up for variables in environment
    feels wrong, but that's to allow re-evaluate body of T_abstractions *)
  let return tv' = make_t ~loc:t.location tv' t.type_meta in
  let other_module = match other_module with 
      Some true -> true
    | _ -> false 
  in
  match t.type_content with
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(evaluate_otype ~raise e) parameters in
    return (T_constant { language; injection; parameters })
  | T_arrow {type1;type2} ->
      let type1 = evaluate_otype ~raise e type1 in
      let type2 = evaluate_otype ~raise e type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : O.row_element) =
        let associated_type = evaluate_otype ~raise e associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      O.LMap.map aux m.content
    in
    let sum : O.rows  = match Environment.get_sum lmap e with
      | None ->
        let layout = m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let () =
      let aux k _v acc = match Environment.get_constructor k e with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then type_
            else if I.LMap.mem (Label "M_left") m.content || I.LMap.mem (Label "M_right") m.content then type_
            else raise.raise (redundant_constructor e k t.location)
          | None -> acc in
      let _ = O.LMap.fold aux m.content ty in ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: O.row_element) =
      let associated_type = evaluate_otype ~raise e associated_type in
      ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let lmap = O.LMap.map aux m.content in
    let record : O.rows = match Environment.get_record lmap e with
    | None ->
      let layout = m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable variable -> (
    let name : O.type_variable = Var.todo_cast variable in
    match Environment.get_type_opt ~other_module name e with
    | Some x -> x
    | None -> (
      match Environment.get_kind_opt name e with
      | Some () -> return (T_variable name)
      | None -> raise.raise (unbound_type_variable e name t.location)
    )
  )
  | T_module_accessor {module_name; element} ->
    let module_ = match Environment.get_module_opt module_name e with
      Some m -> m
    | None   -> raise.raise @@ unbound_module_variable e module_name t.location
    in
    evaluate_otype ~raise ?other_module:(Some true) module_ element
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let env' = Environment.add_kind   x.ty_binder.wrap_content () e in
    let type_ = evaluate_otype ~raise env' x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let env' = Environment.add_type_var x.ty_binder.wrap_content () e in
    let type_ = evaluate_otype ~raise env' x.type_ in
    return (T_for_all {x with type_})

and evaluate_type ~raise ?other_module (e:environment) (t:I.type_expression) : O.type_expression =
  let return tv' = make_t ~loc:t.location tv' (Some t) in
  let other_module = match other_module with 
      Some true -> true
    | _ -> false 
    in
  match t.type_content with
  | T_arrow {type1;type2} ->
      let type1 = evaluate_type ~raise e type1 in
      let type2 = evaluate_type ~raise e type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : I.row_element) =
        let associated_type = evaluate_type ~raise e associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      O.LMap.map aux m.fields
    in
    let sum : O.rows  = match Environment.get_sum lmap e with
      | None ->
        let layout = Option.value ~default:default_layout m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let () =
      let aux k _v acc = match Environment.get_constructor k e with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then type_
            else if I.LMap.mem (Label "M_left") m.fields || I.LMap.mem (Label "M_right") m.fields then type_
            else raise.raise (redundant_constructor e k t.location)
          | None -> acc in
      let _ = O.LMap.fold aux m.fields ty in ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: I.row_element) =
      let associated_type = evaluate_type ~raise e associated_type in
      ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let lmap = O.LMap.map aux m.fields in
    let record : O.rows = match Environment.get_record lmap e with
    | None ->
      let layout = Option.value ~default:default_layout m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable variable -> (
    let name : O.type_variable = Var.todo_cast variable in
    match Environment.get_type_opt ~other_module name e with
    | Some x -> x
    | None when Ast_typed.Helpers.is_generalizable_variable name ->
       (* Case happening when trying to use a variable that is not in
          the environment, but it is generalizable: we hint the user
          that the variable could be put in the extended environment
          via an annotation *)
       raise.raise (not_annotated t.location)
    | None -> (
      match Environment.get_kind_opt name e with
      | Some () -> return (T_variable name)
      | None -> raise.raise (unbound_type_variable e name t.location)
    )
  )
  | T_app {type_operator;arguments} -> (
    let name : O.type_variable = Var.todo_cast type_operator in
    let operator = trace_option ~raise (unbound_type_variable e name t.location) @@
      Environment.get_type_opt ~other_module name e
    in
    let is_fully_applied location (t:O.type_expression) =
      match t.type_content with
      | T_abstraction x ->
        let rec aux : (O.type_expression * int) -> (O.type_expression * int) =
          fun (t,i) -> match t.type_content with T_abstraction x -> aux (x.type_,i+1) | _ -> (t,i)
        in
        let expected = snd @@ aux (x.type_,1) in
        raise.raise (type_constant_wrong_number_of_arguments None expected 0 location)
      | _ -> ()
    in
    let rec aux : O.type_expression * I.type_variable Location.wrap list -> O.type_expression * I.type_variable Location.wrap list =
      fun (op, lst) ->
        match op.type_content with
        | O.T_abstraction {ty_binder;kind=_;type_} -> (
          aux (type_ , lst @ [ty_binder])
        )
        | _ -> (op, lst)
    in
    let (ty_body,vars) = aux (operator, []) in
    let vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise.raise (type_constant_wrong_number_of_arguments (Some name) expected actual t.location)
      | Ok x -> x
    in
    let aux : environment -> (I.type_variable Location.wrap * I.type_expression) -> environment =
      fun env' (ty_binder,arg) ->
        let arg' = evaluate_type ~raise e arg in
        let () = is_fully_applied arg.location arg' in
        let ty_binder : O.type_variable = Var.todo_cast ty_binder.wrap_content in
        Environment.add_type ~public:true ty_binder arg' env'
    in
    let env' = List.fold_left ~f:aux ~init:e vargs in
    match ty_body.type_content with
    | T_constant {language;injection;parameters} ->
      let aux : O.type_expression -> O.type_expression =
        fun t ->
          let t' = evaluate_otype ~raise env' t in
          let () = is_fully_applied t.location t' in
          t'
      in
      let args = List.map ~f:aux parameters in
      return (T_constant {language;injection;parameters=args})
    | _ -> evaluate_otype ~raise env' ty_body
  )
  | T_module_accessor {module_name; element} ->
    let module_ = match Environment.get_module_opt module_name e with
      Some m -> m
    | None   -> raise.raise @@ unbound_module_variable e module_name t.location
    in
    evaluate_type ~raise ?other_module:(Some true) module_ element
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let env' = Environment.add_kind x.ty_binder.wrap_content () e in
    let type_ = evaluate_type ~raise env' x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let env' = Environment.add_type_var x.ty_binder.wrap_content () e in
    let type_ = evaluate_type ~raise env' x.type_ in
    return (T_for_all {x with type_})

and type_expression ~raise ~test ~protocol_version : environment -> ?tv_opt:O.type_expression -> I.expression -> O.environment * O.expression
  = fun e ?tv_opt ae ->
    let res = type_expression' ~raise ~test ~protocol_version e ?tv_opt ae in
    (e, res)

and type_expression' ~raise ~test ~protocol_version ?(args = []) ?last : environment -> ?tv_opt:O.type_expression -> ?other_module:bool -> I.expression -> O.expression = fun e ?tv_opt ?other_module ae ->
  let module L = Logger.Stateful() in
  let return expr tv =
    let () =
      match tv_opt with
      | None -> ()
      | Some tv' -> assert_type_expression_eq ~raise ae.location (tv' , tv) in
    let location = ae.location in
    make_e ~location expr tv in
  let return_e (expr : O.expression) = return expr.expression_content expr.type_expression in
  trace ~raise (expression_tracer ae) @@
  fun ~raise -> match ae.expression_content with
  (* Basic *)
  | E_variable name' ->
      let name = cast_var name' in
      let tv' =
        trace_option ~raise (unbound_variable e name ae.location)
        @@ Environment.get_opt ?other_module name e in
      (match tv'.type_value with
       | { type_content = T_for_all _ } ->
          (* TODO: This is some inference, and we should reconcile it with the inference pass. *)
          let avs, type_ = O.Helpers.destruct_for_alls tv'.type_value in
          let table = infer_type_applications ~raise ~loc:ae.location type_ (List.map ~f:(fun ({type_expression} : O.expression) -> type_expression) args) last in
          let lamb = make_e ~location:ae.location (E_variable name') tv'.type_value in
          return_e @@ build_type_insts ~raise ~loc:ae.location lamb table avs
       | _ ->
          return (E_variable name) tv'.type_value)
  | E_literal Literal_unit ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal (Literal_string s) ->
      return (E_literal (Literal_string s)) (t_string ())
  | E_literal (Literal_key s) ->
      return (E_literal (Literal_key s)) (t_key ())
  | E_literal (Literal_key_hash s) ->
      return (E_literal (Literal_key_hash s)) (t_key_hash ())
  | E_literal (Literal_chain_id s) ->
      return (E_literal (Literal_chain_id s)) (t_chain_id ())
  | E_literal (Literal_signature s) ->
      return (E_literal (Literal_signature s)) (t_signature ())
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_timestamp n) ->
      return (E_literal (Literal_timestamp n)) (t_timestamp ())
  | E_literal (Literal_mutez n) ->
      return (E_literal (Literal_mutez n)) (t_mutez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  | E_literal (Literal_operation op) ->
      return (e_operation op) (t_operation ())
  | E_record_accessor {record;path} ->
      let e' = type_expression' ~raise ~test ~protocol_version e record in
      let aux ~raise (prev:O.expression) (a:I.label) : O.expression =
            let property = a in
            let r_tv = trace_option ~raise (bad_record_access property ae prev.type_expression ae.location) @@
             get_t_record prev.type_expression in
            let tv =
              trace_option ~raise (bad_record_access property ae prev.type_expression ae.location) @@
              O.LMap.find_opt property r_tv.content in
            let location = ae.location in
            make_e ~location (E_record_accessor {record=prev; path=property}) tv.associated_type
      in
      let ae =
      trace ~raise (record_access_tracer e') @@ aux e' path in
      (* check type annotation of the final accessed element *)
      let () =
        match tv_opt with
        | None -> ()
        | Some tv' -> assert_type_expression_eq ~raise ae.location (tv' , ae.type_expression) in
      (ae)
  | E_constructor {constructor = Label s ; element} when String.equal s "M_left" || String.equal s "M_right" -> (
    let t = trace_option ~raise (michelson_or (Label s) ae.location) @@ tv_opt in
    let expr' = type_expression' ~raise ~test ~protocol_version e element in
    ( match t.type_content with
      | T_sum c ->
        let {associated_type ; _} : O.row_element = O.LMap.find (Label s) c.content in
        let () = assert_type_expression_eq ~raise expr'.location (associated_type, expr'.type_expression) in
        return (E_constructor {constructor = Label s; element=expr'}) t
      | _ -> raise.raise (michelson_or (Label s) ae.location)
    )
  )
  (* Sum *)
  | E_constructor {constructor; element} ->
      let (avs, c_tv, sum_tv) = trace_option ~raise (unbound_constructor e constructor ae.location) @@
        Environment.get_constructor_parametric constructor e in
      let expr' = type_expression' ~raise ~test ~protocol_version e element in
      let table = infer_type_application ~raise ~loc:element.location TMap.empty c_tv expr'.type_expression in
      let table = match tv_opt with
        | Some tv_opt -> infer_type_application ~raise ~loc:ae.location ~default_error:(fun loc t t' -> assert_equal loc t' t) table sum_tv tv_opt
        | None -> table in
      let () = trace_option ~raise (not_annotated ae.location) @@
                 if (List.for_all avs ~f:(fun v -> TMap.mem v table)) then Some () else None in
      let c_tv = TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table c_tv in
      let sum_tv = TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table sum_tv in
      let () = assert_type_expression_eq ~raise expr'.location (c_tv, expr'.type_expression) in
      return (E_constructor {constructor; element=expr'}) sum_tv
  (* Record *)
  | E_record m ->
      let m' = O.LMap.map (type_expression' ~raise ~test ~protocol_version e) m in
      let lmap = O.LMap.map (fun e -> ({associated_type = get_type_expression e; michelson_annotation = None; decl_pos=0}:O.row_element)) m' in
      let record_type = match Environment.get_record lmap e with
        | None -> t_record ~layout:default_layout lmap
        | Some (orig_var,r) -> make_t_orig_var (T_record r) None orig_var
      in
      return (E_record m') record_type
  | E_record_update {record; path; update} ->
    let record = type_expression' ~raise ~test ~protocol_version e record in
    let update = type_expression' ~raise ~test ~protocol_version e update in
    let wrapped = get_type_expression record in
    let tv =
      match wrapped.type_content with
      | T_record {content;_} -> (
          let O.{associated_type;_} = trace_option ~raise (bad_record_access path ae wrapped update.location) @@
            O.LMap.find_opt path content in
          associated_type
      )
      | _ -> failwith "Update an expression which is not a record"
    in
    let () = assert_type_expression_eq ~raise update.location (tv, get_type_expression update) in
    return (E_record_update {record; path; update}) wrapped
  (* Data-structure *)
  | E_lambda lambda ->
     let lambda =
         match tv_opt with
         | None -> lambda
         | Some tv' ->
            match O.get_t_function tv' with
            | None -> lambda
            | Some (input_type,_ ) ->
               let input_type = Untyper.untype_type_expression_nofail input_type in
               match lambda.binder.ascr with
               | None -> let binder = {lambda.binder with ascr = Some input_type } in
                         { lambda with binder = binder }
               | Some _ -> lambda in
     let (lambda,lambda_type) = type_lambda ~raise ~test ~protocol_version e lambda in
     return (E_lambda lambda ) lambda_type
  | E_constant {cons_name=( C_LIST_FOLD | C_MAP_FOLD | C_SET_FOLD | C_FOLD) as opname ;
                arguments=[
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ }) as _lambda ;
                    collect ;
                    init_record ;
                  ]} ->
      let open Stage_common.Constant in
      (* this special case is here to force annotation of the untyped lambda
         generated by pascaligo's for_collect loop *)
      let lname = cast_var lname in
      let (v_col , v_initr ) = Pair.map ~f:(type_expression' ~raise ~test ~protocol_version e) (collect , init_record ) in
      let tv_col = get_type_expression v_col   in (* this is the type of the collection  *)
      let tv_out = get_type_expression v_initr in (* this is the output type of the lambda*)
      let input_type = match tv_col.type_content with
        | O.T_constant {language=_ ; injection ; parameters=[t]}
            when String.equal (Ligo_string.extract injection) list_name
              || String.equal (Ligo_string.extract injection) set_name ->
          make_t_ez_record (("0",tv_out)::[("1",t)])
        | O.T_constant {language=_ ; injection ; parameters=[k;v]}
          when String.equal (Ligo_string.extract injection) map_name
            || String.equal (Ligo_string.extract injection) big_map_name ->
          make_t_ez_record (("0",tv_out)::[("1",make_t_ez_record [("0",k);("1",v)])])
        | _ -> raise.raise @@ bad_collect_loop tv_col ae.location in
      let e' = Environment.add_ez_binder lname input_type e in
      let body = type_expression' ~raise ~test ~protocol_version ?tv_opt:(Some tv_out) e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) in
      let lst' = [lambda'; v_col; v_initr] in
      let tv_lst = List.map ~f:get_type_expression lst' in
      let (opname', tv) =
        type_constant ~raise ~test ~protocol_version opname ae.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_FOLD_WHILE as opname;
                arguments = [
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ }) as _lambda ;
                    init_record ;
                ]} ->
      let v_initr = type_expression' ~raise ~test ~protocol_version e init_record in
      let tv_out = get_type_expression v_initr in
      let input_type  = tv_out in
      let lname = cast_var lname in
      let e' = Environment.add_ez_binder lname input_type e in
      let body = type_expression' ~raise ~test ~protocol_version e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) in
      let lst' = [lambda';v_initr] in
      let tv_lst = List.map ~f:get_type_expression lst' in
      let (opname',tv) = type_constant ~raise ~test ~protocol_version opname ae.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_CREATE_CONTRACT as cons_name;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~test ~protocol_version e) arguments in
      let () = match lst' with
        | { expression_content = O.E_lambda l ; _ } :: _ ->
          let open Ast_typed.Misc in
          let fvs = Free_variables.lambda [] l in
          if Int.equal (List.length fvs) 0 then ()
          else raise.raise @@ fvs_in_create_contract_lambda ae (List.hd_exn fvs)
        | _ -> raise.raise @@ create_contract_lambda C_CREATE_CONTRACT ae
      in
      let tv_lst = List.map ~f:get_type_expression lst' in
      let (name', tv) =
        type_constant ~raise ~test ~protocol_version cons_name ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name=C_SET_ADD|C_CONS as cst;arguments=[key;set]} ->
      let key' =  type_expression' ~raise ~test ~protocol_version e key in
      let tv_key = get_type_expression key' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> match cst with
            C_SET_ADD -> t_set tv_key
          | C_CONS -> t_list tv_key
          | _ -> failwith "Only C_SET_ADD and C_CONS are possible because those were the two cases matched above"
      in
      let set' =  type_expression' ~raise ~test ~protocol_version e ~tv_opt:tv set in
      let tv_set = get_type_expression set' in
      let tv_lst = [tv_key;tv_set] in
      let (name', tv) = type_constant ~raise ~test ~protocol_version cst ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';set']}) tv
  | E_constant {cons_name=C_MAP_ADD as cst; arguments=[key;value;map]} ->
      let key' = type_expression' ~raise ~test ~protocol_version e key in
      let val' = type_expression' ~raise ~test ~protocol_version e value in
      let tv_key = get_type_expression key' in
      let tv_val = get_type_expression val' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> t_map_or_big_map tv_key tv_val
      in
      let map' =  type_expression' ~raise ~test ~protocol_version e ~tv_opt:tv map in
      let tv_map = get_type_expression map' in
      let tv_lst = [tv_key;tv_val;tv_map] in
      let (name', tv) = type_constant ~raise ~test ~protocol_version cst ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';val';map']}) tv
  | E_constant {cons_name = C_POLYMORPHIC_ADD;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~test ~protocol_version e) arguments in
      let tv_lst = List.map ~f:get_type_expression lst' in
      let decide = function
        | {O.expression_content = E_literal (Literal_string _); _ } -> Some S.C_CONCAT
        | {expression_content = E_constant {cons_name = C_ADD; _ }; _ } -> Some C_ADD
        | {expression_content = E_constant {cons_name = C_CONCAT; _ }; _ } -> Some C_CONCAT
        | {expression_content = E_constant {cons_name = C_SLICE; _ }; _ } -> Some C_CONCAT
        | {expression_content = E_literal (Literal_int _); _ } -> Some C_ADD
        | {expression_content = E_record_accessor {record; path}; _ } ->
            (let x = get_record_field_type record.type_expression path in
            match x with
            Some s when is_t_string s ->
              Some C_CONCAT
            | _ -> None )
        | {expression_content = E_variable _; type_expression = texpr } ->
            if is_t_string texpr then
              Some C_CONCAT
            else
              None
        | _ -> None in
      let cst =
        Option.value ~default:S.C_ADD @@ List.find_map lst' ~f:decide in
      let (name', tv) =
        type_constant ~raise ~test ~protocol_version cst ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~test ~protocol_version e) arguments in
      let tv_lst = List.map ~f:get_type_expression lst' in
      let (name', tv) =
        type_constant ~raise ~test ~protocol_version cons_name ae.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_application { lamb = ilamb } ->
     (* TODO: This currently does not handle constraints (as those in inference). *)
     (* Get lambda and applications: (..((lamb arg1) arg2) ...) argk) *)
     let lamb, args = I.Helpers.destruct_applications ae in
     (* Type-check all the involved subexpressions *)
     let args = List.map ~f:(type_expression' ~raise ~protocol_version ~test e) args in
     let lamb = type_expression' ~raise ~protocol_version ~test ~args ?last:tv_opt e lamb in
     (* Remove and save prefix for_alls in the lambda *)
     let avs, lamb_type = O.Helpers.destruct_for_alls lamb.type_expression in
     (* Try to infer/check types for the type variables *)
     let table = infer_type_applications ~raise ~loc:ae.location lamb_type (List.map ~f:(fun v -> v.type_expression) args) tv_opt in
     (* Build lambda with type instantiations *)
     let lamb = build_type_insts ~raise ~loc:ae.location lamb table avs in
     (* Re-build term (i.e. re-add applications) *)
     let app = trace_option ~raise (type_error_approximate ~expression:ilamb ~actual:lamb.type_expression) @@
                 O.Helpers.build_applications_opt lamb args in
     return_e app
  (* Advanced *)
  | E_matching {matchee;cases} -> (
    let matchee' = type_expression' ~raise ~test ~protocol_version e matchee in
    let matcheevar = Location.wrap (Var.fresh ()) in
    let aux : (I.expression, I.type_expression) I.match_case -> ((I.type_expression I.pattern * O.type_expression) list * (I.expression * O.environment)) =
      fun {pattern ; body} -> ([(pattern,matchee'.type_expression)], (body,e))
    in
    let eqs = List.map ~f:aux cases in
    let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:ae.location ~type_f:(type_expression' ~test ~protocol_version ?other_module ~args:[] ?last:None) ~body_t:(tv_opt) matcheevar eqs in
    let case_exp = { case_exp with location = ae.location } in
    let x = O.e_let_in matcheevar matchee' case_exp {inline = false; no_mutation = false; public = true ; view= false } in
    return x case_exp.type_expression
  )
  | E_let_in {let_binder = {var ; ascr = None} ; rhs ; let_result; attr } ->
     let rhs = type_expression' ~raise ~protocol_version ~test e rhs in
     let binder = cast_var var in
     let e' = Environment.add_ez_declaration ~public:attr.public binder rhs attr e in
     let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
     return (E_let_in {let_binder = binder; rhs; let_result; attr }) let_result.type_expression
  | E_let_in {let_binder = {var ; ascr = Some tv} ; rhs ; let_result; attr } ->
    let type_env = List.map e.type_environment ~f:(fun { type_variable ; type_ = _ } -> type_variable) in
    let tv = Ast_core.Helpers.generalize_free_vars type_env tv in
    let av, tv = Ast_core.Helpers.destruct_for_alls tv in
    let pre_env = e in
    let env = List.fold_right av ~f:(fun v env -> Environment.add_type_var v () env) ~init:e in
    let tv = evaluate_type ~raise env tv in
    let rhs = type_expression' ~raise ~protocol_version ~test ~tv_opt:tv env rhs in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all (Location.wrap abs_var) () (aux t abs_vars) in
    let type_expression = aux rhs.type_expression (List.rev av) in
    let rhs = { rhs with type_expression } in
    let binder = cast_var var in
    let e' = Environment.add_ez_declaration ~public:attr.public binder rhs attr pre_env in
    let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
    return (E_let_in {let_binder = binder; rhs; let_result; attr }) let_result.type_expression
  | E_type_in {type_binder; _} when Ast_core.Helpers.is_generalizable_variable type_binder ->
    raise.raise (wrong_generalizable ae.location type_binder)
  | E_type_in {type_binder; rhs ; let_result} ->
    let rhs = evaluate_type ~raise e rhs in
    let e' = Environment.add_type ~public:true type_binder rhs e in
    let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
    return (E_type_in {type_binder; rhs; let_result}) let_result.type_expression
  | E_mod_in {module_binder; rhs; let_result} ->
    let env,rhs = type_module ~raise ~protocol_version ~test ~init_env:e rhs in
    let e' = Environment.add_module ~public:true module_binder env e in
    let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
    return (E_mod_in {module_binder; rhs; let_result}) let_result.type_expression
  | E_mod_alias {alias; binders; result} ->
    let aux e binder =
      trace_option ~raise (unbound_module_variable e binder ae.location) @@
      Environment.get_module_opt binder e in
    let env = List.Ne.fold_left aux e binders in
    let e' = Environment.add_module ~public:true alias env e in
    let result = type_expression' ~raise ~test ~protocol_version e' result in
    return (E_mod_alias {alias; binders; result}) result.type_expression
  | E_raw_code {language;code} ->
    let (code,type_expression) = trace_option ~raise (expected_ascription code) @@
      I.get_e_ascription code.expression_content in
    let code = type_expression' ~raise ~test ~protocol_version e code in
    let type_expression = evaluate_type ~raise e type_expression in
    let code = {code with type_expression} in
    return (E_raw_code {language;code}) code.type_expression
  | E_recursive {fun_name; fun_type; lambda} ->
    let type_env = List.map e.type_environment ~f:(fun { type_variable ; type_ = _ } -> type_variable) in
    let av = Ast_core.Helpers.Free_type_variables.type_expression type_env fun_type in
    let fun_name = cast_var fun_name in
    let fun_type = evaluate_type ~raise e fun_type in
    let e' = Environment.add_ez_binder fun_name fun_type e in
    let e' = List.fold_left av ~init:e' ~f:(fun e v -> Environment.add_type_var v () e) in
    let (lambda,_) = type_lambda ~raise ~test ~protocol_version e' lambda in
    return (E_recursive {fun_name;fun_type;lambda}) fun_type
  | E_ascription {anno_expr; type_annotation} ->
    let tv = evaluate_type ~raise e type_annotation in
    let expr' = type_expression' ~raise ~protocol_version ~test ~last:tv ~tv_opt:tv e anno_expr in
    let type_annotation =
      trace_option ~raise (corner_case "merge_annotations (Some ...) (Some ...) failed") @@
      O.merge_annotation
        (Some tv)
        (Some expr'.type_expression)
        O.assert_type_expression_eq in
    (* check type annotation of the expression as a whole (e.g. let x : t = (v : t') ) *)
    let () =
      match tv_opt with
      | None -> ()
      | Some tv' -> assert_type_expression_eq ~raise anno_expr.location (tv' , type_annotation) in
    {expr' with type_expression=type_annotation}
  | E_module_accessor {module_name; element} ->
    let module_env = match Environment.get_module_opt module_name e with
      Some m -> m
    | None   -> raise.raise @@ unbound_module_variable e module_name ae.location
    in
    let element = type_expression' ~raise ~test  ~protocol_version ~args ?last ?tv_opt ?other_module:(Some true) module_env element in
    return (E_module_accessor {module_name; element}) element.type_expression


and type_lambda ~raise ~test ~protocol_version e {
      binder ;
      output_type ;
      result ;
    } =
      let input_type =
        Option.map ~f:(evaluate_type ~raise e) binder.ascr in
      let output_type =
        Option.map ~f:(evaluate_type ~raise e) output_type
      in
      let binder = cast_var binder.var in
      let input_type = trace_option ~raise (missing_funarg_annotation binder) input_type in
      let e' = Environment.add_ez_binder binder input_type e in
      let body = type_expression' ~raise ~test ~protocol_version ?tv_opt:output_type e' result in
      let output_type = body.type_expression in
      (({binder; result=body}:O.lambda),(t_function input_type output_type ()))



and type_constant ~raise ~test ~protocol_version (name:I.constant') (loc:Location.t) (lst:O.type_expression list) (tv_opt:O.type_expression option) : O.constant' * O.type_expression =
  let typer = Constant_typers.constant_typers ~raise ~test ~protocol_version loc name in
  let tv = typer lst tv_opt in
  (name, tv)

let untype_literal (l:O.literal) : I.literal =
  let open I in
  match l with
  | Literal_unit -> Literal_unit
  | Literal_nat n -> (Literal_nat n)
  | Literal_timestamp n -> (Literal_timestamp n)
  | Literal_mutez n -> (Literal_mutez n)
  | Literal_int n -> (Literal_int n)
  | Literal_string s -> (Literal_string s)
  | Literal_signature s -> (Literal_signature s)
  | Literal_key s -> (Literal_key s)

  | Literal_key_hash s -> (Literal_key_hash s)
  | Literal_chain_id s -> (Literal_chain_id s)
  | Literal_bytes b -> (Literal_bytes b)
  | Literal_address s -> (Literal_address s)
  | Literal_operation s -> (Literal_operation s)

let rec untype_type_expression (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       v' in
     let x' = I.LMap.map aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = I.LMap.map aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_arrow arr ->
    let arr = Stage_common.Maps.arrow self arr in
    return @@ I.T_arrow arr
  | O.T_constant {language=_;injection;parameters} ->
    let arguments = List.map ~f:self parameters in
    let type_operator = Var.of_name (Ligo_string.extract injection) in
    return @@ I.T_app {type_operator;arguments}
  | O.T_module_accessor ma ->
    let ma = Stage_common.Maps.module_access self ma in
    return @@ I.T_module_accessor ma
  | O.T_singleton l ->
    return @@ I.T_singleton l
  | O.T_abstraction x ->
    let type_ = untype_type_expression x.type_ in
    return @@ T_abstraction {x with type_}
  | O.T_for_all x ->
    let type_ = untype_type_expression x.type_ in
    return @@ T_for_all {x with type_}

let rec untype_expression (e:O.expression) : I.expression =
  untype_expression_content e.type_expression e.expression_content
and untype_expression_content ty (ec:O.expression_content) : I.expression =
  let open I in
  let return e = e in
  match ec with
  | E_literal l ->
      let l = untype_literal l in
      return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:untype_expression arguments in
      return (e_constant cons_name lst')
  | E_variable n ->
      let n = cast_var n in
      return (e_variable (n))
  | E_application {lamb;args} ->
      let f' = untype_expression lamb in
      let arg' = untype_expression args in
      return (e_application f' arg')
  | E_lambda {binder ; result} -> (
      let binder = cast_var binder in
      let _, ty = Ast_typed.Helpers.destruct_for_alls ty in
      let io = O.get_t_function_exn ty in
      let (input_type , output_type) =
        Pair.map   ~f:Untyper.untype_type_expression io in
      let result = untype_expression result in
      return (e_lambda {var=binder;ascr=Some input_type;attributes=Stage_common.Helpers.empty_attribute} (Some output_type) result)
    )
  | E_constructor {constructor; element} ->
      let p' = untype_expression element in
      return (e_constructor constructor p')
  | E_record r ->
    let r' = LMap.map untype_expression r in
    return (e_record r')
  | E_record_accessor {record; path} ->
      let r' = untype_expression record in
      let Label s = path in
      return (e_record_accessor r' (Label s))
  | E_record_update {record=r; path=Label l; update=e} ->
    let r' = untype_expression r in
    let e = untype_expression e in
    return (e_record_update r' (I.Label l) e)
  | E_matching {matchee;cases} -> (
    let matchee = untype_expression matchee in
    match cases with
    | Match_variant {cases ; tv} ->
      (*
        If one day this code is actually executed, and if the list type is still not a tuple type.
        A special case for lists might be required here
      *)
      let aux : Ast_typed.matching_content_case -> _ match_case =
        fun { constructor ; pattern ; body } -> (
          let pattern =
            match tv with
            | _ ->
              let proj = Location.wrap @@ P_var { ascr = None ; var = (cast_var pattern) ; attributes = Stage_common.Helpers.empty_attribute } in
              Location.wrap @@ P_variant (constructor, proj)
          in
          let body = untype_expression body in
          ({pattern ; body } : (Ast_core.expression, Ast_core.type_expression) match_case)
        )
      in
      let cases = List.map ~f:aux cases in
      return (e_matching matchee cases)
    | Match_record {fields ; body ; tv=_} -> (
      let aux : (Ast_typed.label * (Ast_typed.expression_variable * _)) -> label * Ast_core.type_expression pattern =
        fun (Ast_typed.Label label, (proj,_)) -> (
          let proj = Location.wrap @@ P_var { ascr = None ; var = (cast_var proj) ; attributes = Stage_common.Helpers.empty_attribute } in
          (Label label, proj)
        )
      in
      let (labels,patterns) = List.unzip @@ List.map ~f:aux (LMap.to_kv_list fields) in
      let body = untype_expression body in
      let case = match Ast_typed.Helpers.is_tuple_lmap fields with
        | false ->
          let pattern = Location.wrap (P_record (labels,patterns)) in
          ({ pattern ; body } : _ Ast_core.match_case)
        | true ->
          let pattern = Location.wrap (P_tuple patterns) in
          ({ pattern ; body } : _ Ast_core.match_case)
      in
      return (e_matching matchee [case])
    )
  )
  | E_let_in {let_binder;rhs;let_result; attr} ->
      let let_binder = cast_var let_binder in
      let tv = Untyper.untype_type_expression rhs.type_expression in
      let rhs = untype_expression rhs in
      let result = untype_expression let_result in
      return (e_let_in {var=let_binder ; ascr=(Some tv) ; attributes = Stage_common.Helpers.empty_attribute} rhs result attr)
  | E_type_in {type_binder;rhs;let_result} ->
      let rhs = Untyper.untype_type_expression rhs in
      let let_result = untype_expression let_result in
      return @@ make_e @@ E_type_in {type_binder; rhs; let_result }
  | E_mod_in {module_binder;rhs;let_result} ->
      let rhs = untype_module_fully_typed rhs in
      let result = untype_expression let_result in
      return @@ e_mod_in module_binder rhs result
  | E_mod_alias ma ->
      let ma = Stage_common.Maps.mod_alias untype_expression ma in
      return @@ make_e @@ E_mod_alias ma
  | E_raw_code {language; code} ->
      let code = untype_expression code in
      return (e_raw_code language code)
  | E_recursive {fun_name;fun_type; lambda} ->
      let fun_name = cast_var fun_name in
      let fun_type = Untyper.untype_type_expression fun_type in
      let unty_expr= untype_expression_content ty @@ E_lambda lambda in
      let lambda = match unty_expr.expression_content with I.E_lambda l -> l | _ -> failwith "impossible case" in
      return @@ e_recursive fun_name fun_type lambda
  | E_module_accessor ma ->
    let ma = Stage_common.Maps.module_access untype_expression ma in
    return @@ I.make_e @@ E_module_accessor ma
  | E_type_inst {forall;type_=type_inst} ->
    match forall.type_expression.type_content with
    | T_for_all {ty_binder;type_} ->
       let type_ = Ast_typed.Helpers.subst_type (Location.unwrap ty_binder) type_inst type_ in
       let forall = { forall with type_expression = type_ } in
       untype_expression forall
    | _ ->
     failwith "Impossible case: cannot untype a type instance of a non polymorphic type"

and untype_declaration : O.declaration -> I.declaration =
let return (d: I.declaration) = d in
function
  Declaration_type {type_binder; type_expr;type_attr={public}} ->
  let type_expr = untype_type_expression type_expr in
  return @@ Declaration_type {type_binder; type_expr;type_attr={public}}
| Declaration_constant {name; binder;expr;attr={inline;no_mutation;public;view}} ->
  let ty = untype_type_expression expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let expr = untype_expression expr in
  return @@ Declaration_constant {name; binder={var;ascr=Some ty;attributes = Stage_common.Helpers.empty_attribute};expr;attr={inline;no_mutation;view;public}}
| Declaration_module {module_binder;module_;module_attr={public}} ->
  let module_ = untype_module_fully_typed module_ in
  return @@ Declaration_module {module_binder;module_; module_attr={public}}
| Module_alias ma ->
  return @@ Module_alias ma

and untype_module_fully_typed : O.module_fully_typed -> I.module_ = fun (Module_Fully_Typed m) ->
  List.map ~f:(Location.map untype_declaration) m

let rec decompile_env (env : Ast_typed.environment) =
  let expression_environment = List.map ~f:decompile_binding env.expression_environment in
  let type_environment       = List.map ~f:decompile_type_binding env.type_environment in
  let module_environment     = List.map ~f:decompile_module_binding env.module_environment in
  Ast_core.{expression_environment; type_environment; module_environment}

and decompile_binding Ast_typed.{expr_var;env_elt} =
  let type_value = untype_type_expression env_elt.type_value in
  let definition = match env_elt.definition with
    ED_binder -> Ast_core.ED_binder
  | ED_declaration {expression;free_variables} ->
    let expression = untype_expression expression in
    Ast_core.ED_declaration {expression;free_variables}
  in
  Ast_core.{expr_var;env_elt = {type_value;definition}}

and decompile_type_binding Ast_typed.{type_variable;type_} =
  let type_ =
    match type_ with
    | Ty type_ -> untype_type_expression type_
    | Kind () -> Ast_core.t_variable type_variable
  in
  Ast_core.{type_variable;type_}

and decompile_module_binding Ast_typed.{module_variable;module_} =
  let module_ = decompile_env module_ in
  Ast_core.{module_variable;module_}
