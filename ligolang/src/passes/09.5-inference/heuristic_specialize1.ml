(* selector / propagation rule for specializing polymorphic types
 * For now: (x = forall y, z) and (x = k'(var' …))
 * produces the new constraint (x = z[y ↦ fresh_y])
 * where [from ↦ to] denotes substitution. *)

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t  Grouped_by_variable.t
  end
end

module Core = Typesystem.Core
open Solver_types
open Typer_common.Errors
open Ast_core.Reasons

(* TODO: we need to detect if a ∀ constraint has already been specialized or not
   The same need was present for the heuristic_tc_fundep (detect if a TC has already
   been refined, and if so find the update) *)
 
module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  type type_variable = Type_variable.t


  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins

  type selector_output = {
      poly : c_poly_simpl ;
      a_k_var : c_constructor_simpl ;
    }
  let heuristic_name = "specialize1"

 let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  (* find two rules with the shape (x = forall b, d) and x = k'(var' …) or vice versa *)
  (* TODO: do the same for two rules with the shape (a = forall b, d) and tc(a…) *)
  (* TODO: do the appropriate thing for two rules with the shape (a = forall b, d) and (a = forall b', d') *)
  fun repr type_constraint_simpl ((module Indexes) : flds)->
  match type_constraint_simpl with
    SC_Apply _ -> []
  | SC_Abs   _ -> []
  | SC_Constructor c                ->
    (* vice versa *)
    let other_cs = MultiSet.elements @@ Grouped_by_variable.get_polys_by_lhs (repr c.tv) Indexes.grouped_by_variable in
    let cs_pairs = List.map ~f:(fun x -> { poly = x ; a_k_var = c }) other_cs in
    cs_pairs
  | SC_Alias       _                -> failwith "alias should not be visible here"
  | SC_Poly        p                ->
    let other_cs = MultiSet.elements @@ Grouped_by_variable.get_constructors_by_lhs (repr p.tv) Indexes.grouped_by_variable in
    let cs_pairs = List.map ~f:(fun x -> { poly = p ; a_k_var = x }) other_cs in
    cs_pairs
  | SC_Typeclass   _                -> []
  | SC_Access_label _               -> []
  | SC_Row _                        -> []

(* when α = ∀ δ, γ and β = κ(ε, …) are in the db, aliasing α and β
   should check they are non-empty (and in that case produce a
   selector_output for all pairs) *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_polys = MultiSet.elements @@ Grouped_by_variable.get_polys_by_lhs a Indexes.grouped_by_variable in
  let a_ctors = MultiSet.elements @@ Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
  let b_polys = MultiSet.elements @@ Grouped_by_variable.get_polys_by_lhs b Indexes.grouped_by_variable in
  let b_ctors = MultiSet.elements @@ Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  List.concat @@
  List.map
    ~f:(fun poly ->
       List.map
         ~f:(fun ctor ->
            { poly ; a_k_var = ctor })
         (a_ctors @ b_ctors))
    (a_polys @ b_polys)

let get_referenced_constraints ({ poly; a_k_var } : selector_output) : type_constraint_simpl list =
  [
    SC_Poly poly;
    SC_Constructor a_k_var;
  ]

let propagator : (selector_output , typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise:_ selected repr ->
  (* Format.eprintf "In specialize propagator for %a\n%!" pp_selector_output selected; *)
  let a = selected.poly in
  let b = selected.a_k_var in

  (* The selector is expected to provide two constraints with the shape (x = forall y, z) and x = k'(var' …) *)
  assert (Type_variable_abstraction.Compare.type_variable (repr (a : c_poly_simpl).tv) (repr (b : c_constructor_simpl).tv) = 0);

  (* produce constraints: *)

  (* create a fresh existential variable to instantiate the polymorphic type y *)
  let fresh_existential = Core.fresh_type_variable () in
  (* Produce the constraint (b.tv = a.body[a.binder |-> fresh_existential])
     The substitution is obtained by immediately applying the forall. *)
  (* TODO: this should not use apply, universally-quantified types are *not* type-level functions, even though the substitution is identical on both. *)
  let apply =
    wrap Propagator_specialize_apply @@
      P_apply { tf   = wrap Propagator_specialize_tf @@ P_forall a.forall ;
                targ = wrap Propagator_specialize_targ @@ P_variable fresh_existential ;
      }
  in
  let (reduced, new_constraints) = Typelang.check_applied @@ Typelang.type_level_eval apply in
  
  let eq1 = Misc.c_equation (wrap (Todo "solver: propagator: specialize1 eq1") @@ P_variable (repr b.tv)) reduced "propagator: specialize1" in
  let eqs = eq1 :: new_constraints in
    [
        {
          remove_constraints = [ SC_Poly a ];
          add_constraints = eqs;
          add_constraints_simpl = [];
          proof_trace = Axiom Axioms.specialize
        }
      ]

let printer ppf ({poly;a_k_var}) =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppf "%a = %a"
    c_poly_simpl_short poly
    c_constructor_simpl_short a_k_var
let printer_json ({poly;a_k_var}) =
  let open Type_variable_abstraction.Yojson in
  `Assoc [
    ("poly",    c_poly_simpl poly);
    ("a_k_var", c_constructor_simpl a_k_var)]
let comparator { poly = a1; a_k_var = a2 } { poly = b1; a_k_var = b2 } =
  let open Type_variable_abstraction.Compare in
  c_poly_simpl a1 b1 <? fun () -> c_constructor_simpl a2 b2
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)

open Ast_core.Types
open Solver_types

module Compat = struct
  include MM
  open All_plugins
  let compat_flds flds : MM.flds = (module struct
    let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
  end)
  let selector repr c flds = MM.selector repr c (compat_flds flds)
  let alias_selector a b flds = MM.alias_selector a b (compat_flds flds)
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }
type nonrec selector_output = MM.selector_output = {
      poly : c_poly_simpl ;
      a_k_var : c_constructor_simpl ;
    }
let selector = Compat.selector
let propagator = Compat.propagator
let comparator = Compat.comparator
