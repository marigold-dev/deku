(* selector / propagation rule for applying type-level functions
   in typeclasses. This is a quick&dirty solution, waiting for the
   ∃ to be moved within the types instead of on the typeclasses.

   Given a typeclass
     (α…) ∈ ∃δ…, …,φ(β),… => [ …,β,… ]
   and a second second hypothesis of the form
     φ = λα.τ
   It inlines the definition of the type-level function φ,
   and performs beta-reduction, to obtain:
     (α…) ∈ ∃δ…, …,τ[α↦β],… => [ …,β,… ]
   where τ[α↦β] denotes the substitution of α by β in τ. *)

open Typer_common.Errors
open Simple_utils

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t  Grouped_by_variable.t
    val assignments : Type_variable.t Assignments.t
    val typeclasses_using_as_function_on_root : Type_variable.t Typeclasses_using_as_function_on_root.t
  end
end

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  (* open Type_variable_abstraction.Reasons *)

  module Utils = Heuristic_tc_utils.Utils(Type_variable)(Type_variable_abstraction)
  open Utils
  open Utils.All_plugins

  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)

  type selector_output = {
    tc : c_typeclass_simpl ;
    f :  c_abs_simpl ;
  }

  let heuristic_name = "tc_beta_type_abs"
  
(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* selector:
 *   find in db φ = λα.τ and (…) ∈ ∃δ…, …,φ(β),… => [ …,β,… ]
 *)
  
(* Find typeclass constraints in the dbs which calls f (passing one of the tc's roots as an argument) *)
let selector_by_abs : (type_variable -> type_variable) -> flds -> c_abs_simpl -> selector_output list =
  fun repr (module Indexes) f ->
  let typeclasses = Typeclasses_using_as_function_on_root.get_list (repr f.tv) (module Indexes) in
  List.map ~f:(fun tc -> { tc ; f }) typeclasses

(* Find all abs constraints φ = λα.τ where φ(β) occurs in
   the root of one of the nested constraints of tc, and
   β occurs at the root of one of the cells of the
   matrix of the typeclass. *)
let selector_by_tc : (type_variable -> type_variable) -> flds -> c_typeclass_simpl -> selector_output list =
  fun repr (module Indexes) tc ->
  let tvlist = Typeclasses_using_as_function_on_root.functions_on_roots repr tc in
  List.concat @@ List.map ~f:(fun f ->
    (* TODO: Inefficient: a single Assignment should be enough instead of an entire list. *)
    MultiSet.map_elements (fun ff -> { tc; f=ff})
    (Grouped_by_variable.get_abs_by_lhs (repr f) Indexes.grouped_by_variable))
    tvlist

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun repr type_constraint_simpl indexes ->
  match type_constraint_simpl with
    SC_Apply        _  -> []
  | SC_Abs          a  -> selector_by_abs repr indexes a
  | SC_Constructor  _  -> []
  | SC_Row          _  -> []
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass    tc -> selector_by_tc repr indexes tc

(* φ = λα.τ and (…) ∈ ∃δ…, …,ψ(β),… => [ …,β,… ] are in the db,
   aliasing φ and ψ should produce a selector_output for these
   two constraints *)
let alias_selector_half : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_tcs = Typeclasses_using_as_function_on_root.get a (module Indexes) in
  let b_lhs_abs = Grouped_by_variable.get_abs_by_lhs b Indexes.grouped_by_variable in
  List.concat @@
  MultiSet.map_elements
    (fun tc ->
       MultiSet.map_elements (fun f -> { tc ; f }) b_lhs_abs)
    a_tcs

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b indexes ->
  alias_selector_half a b indexes @ alias_selector_half b a indexes

let get_referenced_constraints ({ tc; f } : selector_output) : type_constraint_simpl list =
  [
    SC_Typeclass tc;
    SC_Abs f;
  ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let p_variable v = Location.wrap (* ~loc: TODO *) @@ P_variable v
let tv : constructor_or_row -> type_variable =
  function `Constructor c -> c.tv | `Row r -> r.tv
let row_value v = { associated_value = p_variable v.associated_variable; michelson_annotation = v.michelson_annotation ; decl_pos = v.decl_pos; }

(*
let susbt_in_body ~old ~update (tv : type_value) =
  let rec susbt_in_body (tv : type_value) =
    let return x = { Location.wrap_content = x; location = tv.location } in
    match tv.wrap_content with
      P_forall { binder; constraints; body } -> if Compare.type_variable old binder = 0 then tv else return @@ P_forall { binder; constraints = subst_in_constraints constraints ; body = susbt_in_body body }
    | P_variable _ -> failwith "ll"
    | P_constant _ -> failwith "ll"
    | P_apply _ -> failwith "ll"
    | P_row _ -> failwith "ll"
    | P_abs _ -> failwith "ll"
    | P_constraint _ -> failwith "ll"
  and subst_in_constraints (constraints : p_constraints) =
    List.map ~f:subst_in_type_constraint constraints
  and subst_in_type_constraint (tc : type_constraint) =
    { reason = tc.reason; c = subst_in_type_constraint_ tc.c}
  and subst_in_type_constraint_ = function
      C_equation { aval; bval } -> ??
    | C_typeclass { tc_bound; tc_constraints; tc_args; original_id; typeclass } -> ??
    | C_access_label { c_access_label_record_type; accessor; c_access_label_tvar } -> ??
  in susbt_in_body tv*)

let subst_in_body ~old ~update (tv:type_value) =
  Type_variable_abstraction.Substitution.Pattern.type_value ~tv ~substs:(old, Location.wrap @@ P_variable update)

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise:_ selected repr ->
  let { tc; f=def } = selected in
  let beta changed = function
      SC_Apply use when (Compare.type_variable (repr use.f) (def.tv) = 0) ->
      (match (subst_in_body ~old:def.param ~update:use.arg def.body).wrap_content with
        | P_constraint {pc} ->
          let do_not_do_this = (Type_variable_abstraction.cast_access_to_simplifier_do_not_do_this_do_not_use_this Simplifier.type_constraint_simpl pc) in
          true, do_not_do_this
        |  _ -> failwith "kind error: expected a value of kind Constraint but got another kind")
    | other -> changed, [other]
  in
  let changed, updated_tc_constraints = List.fold_map ~f:beta ~init:false tc.tc_constraints in
  if not changed then
    []
  else
    [{
      remove_constraints = [SC_Typeclass selected.tc];
      add_constraints = [];
      add_constraints_simpl = [SC_Typeclass { tc with tc_constraints = List.concat updated_tc_constraints }];
      proof_trace = Axiom (HandWaved "unfold")
    }]

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)
    
let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppd "%a and %a" c_typeclass_simpl_short t.tc c_abs_simpl_short t.f

let printer_json (t : selector_output) =
  let open Type_variable_abstraction.Yojson in
  `Assoc [
    ("tc" , c_typeclass_simpl t.tc) ;
    ("f"  , c_abs_simpl       t.f)
  ]
let comparator { tc=a1; f=a2 } { tc=b1; f=b2 } =
  let open Type_variable_abstraction.Compare in
  c_typeclass_simpl a1 b1 <? fun () -> c_abs_simpl a2 b2
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)



open Ast_typed.Types
open Solver_types

module Compat = struct
  module All_plugins = Database_plugins.All_plugins.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
  open All_plugins
  include MM
  let compat_flds flds : MM.flds = (module struct
    let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
    let assignments : type_variable Assignments.t = flds#assignments
    let typeclasses_using_as_function_on_root : type_variable Typeclasses_using_as_function_on_root.t = flds#typeclasses_using_as_function_on_root
  end)
  let selector repr c flds = MM.selector repr c (compat_flds flds)
  let alias_selector a b flds = MM.alias_selector a b (compat_flds flds)
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }
