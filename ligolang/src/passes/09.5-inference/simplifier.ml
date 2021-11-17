module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
open Ast_core.Misc
open Ast_core.Types
open Ast_core.Reasons

(* TODO: make this be threaded around (not essential, but we tend to
   avoid mutable stuff). *)
let global_next_constraint_id : int64 ref = ref 0L

(** This function converts constraints from type_constraint to
    type_constraint_simpl. The former has more possible cases, and the
    latter uses a more minimalistic constraint language.
*)

let rec type_constraint_simpl : type_constraint -> type_constraint_simpl list =
  fun new_constraint ->
  let equal_via_fresh a b =
    let fresh = Core.fresh_type_variable () in
    let cs1 = type_constraint_simpl (c_equation (wrap (Todo "solver: simplifier: simpl 1") @@ P_variable fresh) a "simplifier: simpl 1") in
    let cs2 = type_constraint_simpl (c_equation (wrap (Todo "solver: simplifier: simpl 2") @@ P_variable fresh) b "simplifier: simpl 2") in
    cs1 @ cs2 in
  let access_label_via_fresh ~tv ~record_type ~label = (* α = τ.label  via  β = τ && α = β.label *)
    let fresh = Core.fresh_type_variable () in
    let cs1 = type_constraint_simpl (c_equation (wrap (Todo "solver: simplifier: simpl target of label access") @@ P_variable fresh) record_type "simplifier: simpl target of label access") in
    let id_access_label_simpl = ConstraintIdentifier.fresh () in
    let cs2 = [SC_Access_label { id_access_label_simpl; tv; record_type = fresh; label;reason_access_label_simpl= "simplifier: simpl label access on record via a fresh var for the record's type" }] in
    cs2 @ cs1 in
  let apply_fresh ~f ~arg =
    let id_apply_simpl = ConstraintIdentifier.fresh () in
    [SC_Apply {id_apply_simpl; f; arg;reason_apply_simpl = "solver: simplifier : simpl apply" }] in
  let split_constant a c_tag args =
    let fresh_vars = List.map ~f:(fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map ~f:(fun (v,t) -> c_equation (wrap (Todo "solver: simplifier: split_constant") @@ P_variable v) t "simplifier: split_constant") (List.zip_exn fresh_vars args) in
    let recur = List.map ~f:type_constraint_simpl fresh_eqns in
    let id_constructor_simpl = ConstraintIdentifier.fresh () in
    SC_Constructor {id_constructor_simpl;original_id=None;tv=a;c_tag;tv_list=fresh_vars;reason_constr_simpl=Format.asprintf "simplifier: split constant %a = %a (%a)" Var.pp a Ast_core.PP.constant_tag c_tag (PP_helpers.list_sep Ast_core.PP.type_value (fun ppf () -> Format.fprintf ppf ", ")) args} :: List.concat recur in
  let split_row a r_tag args =
    let aux _ {associated_value = v;michelson_annotation;decl_pos} const =
      let var = Core.fresh_type_variable () in
      let v   = c_equation (wrap (Todo "solver: simplifier: split_row") @@ P_variable var) v "simplifier: split_row" in
      (v::const, {associated_variable=var;michelson_annotation;decl_pos})
    in
    let fresh_eqns, fresh_vars = LMap.fold_map ~f:aux ~init:[] args in
    let recur = List.map ~f:type_constraint_simpl fresh_eqns in
    let id_row_simpl = ConstraintIdentifier.fresh () in
    [SC_Row {id_row_simpl;original_id=None;tv=a;r_tag;tv_map=fresh_vars;reason_row_simpl=Format.asprintf "simplifier: split constant %a = %a (%a)" Var.pp a Ast_core.PP.row_tag r_tag (Ast_core.PP.record_sep Ast_core.PP.row_value (fun ppf () -> Format.fprintf ppf ", ")) args}] @ List.concat recur in
  let gather_forall a forall = 
    let id_poly_simpl = ConstraintIdentifier.fresh () in
    [SC_Poly {id_poly_simpl; original_id=None; tv=a; forall ; reason_poly_simpl="simplifier: gather_forall"}] in
  let gather_alias a b =
    if Var.equal a b
    then [] (* Don't include trivial aliases. *)
    else [SC_Alias { a ; b ; reason_alias_simpl="simplifier: gather_alias"}] in
  let reduce_type_app a b =
    let (reduced, new_constraints) = Solver_types.Typelang.check_applied @@ Solver_types.Typelang.type_level_eval b in
    let recur = List.map ~f:type_constraint_simpl new_constraints in
    let resimpl = type_constraint_simpl (c_equation a reduced "simplifier: reduce_type_app") in (* Note: this calls recursively but cant't fall in the same case. *)
    resimpl @ List.concat recur in
  let split_typeclass tc_bound tc_constraints args tc original_id =
    let fresh_vars = List.map ~f:(fun _ -> Core.fresh_type_variable ()) args in
    let fresh_eqns = List.map ~f:(fun (v,t) -> c_equation (wrap (Todo "solver: simplifier: split typeclass") @@ P_variable v) t "simplifier: split_typeclass") (List.zip_exn fresh_vars args) in
    let recur = List.map ~f:type_constraint_simpl fresh_eqns in
    let id_typeclass_simpl = ConstraintIdentifier.fresh () in
    (* TODO: potential bug: I'm not sure if something needs to be done about the bound variables. It's probably okay as-is. *)
    let tc_constraints_simpl = List.concat @@ List.map ~f:type_constraint_simpl tc_constraints in
    global_next_constraint_id := Int64.add !global_next_constraint_id 1L;
    [SC_Typeclass { tc_bound; tc_constraints = tc_constraints_simpl; tc ; args = fresh_vars ; id_typeclass_simpl ; original_id; reason_typeclass_simpl="simplifier: split_typeclass"}] @ List.concat recur in

  match new_constraint.c with
  (* break down (forall 'b, body = forall 'c, body') into ('a = forall 'b, body and 'a = forall 'c, body')) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_forall _ } as a); bval=({ location = _ ; wrap_content = P_forall _ } as b)}     -> equal_via_fresh a b
  (* break down (forall 'b, body = c(args)) into ('a = forall 'b, body and 'a = c(args)) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_forall _ } as a); bval=({ location = _ ; wrap_content = P_constant _ } as b)}   -> equal_via_fresh a b
  (* break down (forall 'b, body = r(args)) into ('a = forall 'b, body and 'a = r(args)) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_forall _ } as a); bval=({ location = _ ; wrap_content = P_row _ } as b)}   -> equal_via_fresh a b
  (* break down (c(args) = c'(args')) into ('a = c(args) and 'a = c'(args')) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_constant _ } as a); bval=({ location = _ ; wrap_content = P_constant _ } as b)} -> equal_via_fresh a b
  (* break down (r(args) = r'(args')) into ('a = r(args) and 'a = r'(args')) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_row _ } as a); bval=({ location = _ ; wrap_content = P_row _ } as b)} -> equal_via_fresh a b
  (* break down (c(args) = forall 'b, body) into ('a = c(args) and 'a = forall 'b, body) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_constant _ } as a); bval=({ location = _ ; wrap_content = P_forall _ } as b)}   -> equal_via_fresh a b
  (* break down (r(args) = forall 'b, body) into ('a = r(args) and 'a = forall 'b, body) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_row _ } as a); bval=({ location = _ ; wrap_content = P_forall _ } as b)}   -> equal_via_fresh a b
  (* break down (r(args) = c(args')) into ('a = r(args) and 'a = c(args')) *)
  | C_equation {aval=({ location = _ ; wrap_content = P_constant _} as a); bval = ({ location = _ ; wrap_content = P_row _} as b)}
  | C_equation {aval=({ location = _ ; wrap_content = P_row _} as a); bval = ({ location = _ ; wrap_content = P_constant _} as b)} -> equal_via_fresh a b
  | C_equation {aval={ location = _ ; wrap_content = P_forall forall }; bval={ location = _ ; wrap_content = P_variable b }}        -> gather_forall b forall
  | C_equation {aval={ location = _ ; wrap_content = P_variable a }; bval={ location = _ ; wrap_content = P_forall forall }}            -> gather_forall a forall
  | C_equation {aval={ location = _ ; wrap_content = P_variable a }; bval={ location = _ ; wrap_content = P_variable b }}               -> gather_alias a b
  | C_equation {aval={ location = _ ; wrap_content = P_variable a }; bval={ location = _ ; wrap_content = P_constant { p_ctor_tag; p_ctor_args } }}
  | C_equation {aval={ location = _ ; wrap_content = P_constant {p_ctor_tag; p_ctor_args} }; bval={ location = _ ; wrap_content = P_variable a }}   -> split_constant a p_ctor_tag p_ctor_args
  | C_equation {aval={ location = _ ; wrap_content = P_variable a }; bval={ location = _ ; wrap_content = P_row { p_row_tag; p_row_args } }}
  | C_equation {aval={ location = _ ; wrap_content = P_row { p_row_tag; p_row_args }}; bval={ location = _ ; wrap_content = P_variable a}} -> split_row a p_row_tag p_row_args
  (*  Reduce the type-level application, and simplify the resulting constraint + the extra constraints (typeclasses) that appeared at the forall binding site *)
  | C_equation {aval=(_ as a); bval=({ location = _ ; wrap_content = P_apply _ } as b)}               -> reduce_type_app a b
  | C_equation {aval=({ location = _ ; wrap_content = P_apply _ } as a); bval=(_ as b)}               -> reduce_type_app b a
  (* break down (TC(args)) into (TC('a, …) and ('a = arg) …) *)
  | C_typeclass { tc_bound; tc_constraints; tc_args; typeclass; original_id }                         -> split_typeclass tc_bound tc_constraints tc_args typeclass original_id
  | C_access_label { c_access_label_record_type; accessor; c_access_label_tvar } -> access_label_via_fresh ~tv:c_access_label_tvar ~record_type:c_access_label_record_type ~label:accessor
  | C_equation {aval={ location = _; wrap_content = P_abs _ | P_constraint _};bval=_} -> failwith "unimplemented"
  | C_equation {aval=_;bval={ location = _; wrap_content = P_abs _ | P_constraint _}} -> failwith "unimplemented"
  | C_apply {f;arg} -> apply_fresh ~f ~arg
