(* selector / propagation rule for inlining type variables which
   refer to other types.

   Given a typeclass
     (α₁, α₂, …) ∈ ∃δ…, c… => { (τ₁₁, τ₁₂, …) , … }
   where there are one or more τᵢⱼ = P_variable γ, and a second
   second hypothesis of the form
     γᵢ = κ(β₁, β₂, …)
      or
     γᵢ = Ξ(ℓᵢ : βᵢ, …)
   It inlines the definition of the type γᵢ.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor or row; it will then inline
   the arguments of the constructor or row, and continue inferring
   until the typeclass is in a minimal form. *)

open Typer_common.Errors

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  module type S = sig
  end
end

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction.Types

  module Utils = Heuristic_tc_utils.Utils(Type_variable)(Type_variable_abstraction)
  open Utils

  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)

  type selector_output = {
    tc : c_typeclass_simpl ;
  }

  let heuristic_name = "tc_fundep"
  
(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* selector:
 *   find in db any typeclass (α…) ∈ ∃δ…, c… => [ (τ…) … ] *)
  
let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun _repr type_constraint_simpl _indexes ->
  match type_constraint_simpl with
    SC_Apply        _  -> []
  | SC_Abs          _  -> []
  | SC_Constructor  _  -> []
  | SC_Row          _  -> []
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass    tc -> [{ tc }]

let alias_selector_half : type_variable -> type_variable -> flds -> selector_output list =
  fun _a _b (module Indexes) ->
  []

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b indexes ->
  alias_selector_half a b indexes @ alias_selector_half b a indexes

let get_referenced_constraints ({ tc; } : selector_output) : type_constraint_simpl list =
  [ SC_Typeclass tc ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise selected repr ->
  let open Type_variable_abstraction.Misc in
  let open Type_variable_abstraction.Reasons in
  let ( deduced , cleaned , changed ) = wrapped_deduce_and_clean ~raise repr selected.tc ~original:selected.tc in
  let deduced_single_line = (match cleaned with
   SC_Typeclass {tc=[line];args;tc_bound} -> List.filter_map ~f:(function Location.{wrap_content = P_variable v},a when not @@ List.mem ~equal:Caml.(=) tc_bound v -> Some (c_equation
          (wrap (Propagator_break_ctor "v") @@ P_variable (repr v))
          (wrap (Propagator_break_ctor "a") @@ P_variable (repr a))
          "deduce_and_cleaned:single_line"
) | _ -> None) @@ List.zip_exn line args
         | _ -> []) in
  if not changed then
    []
  else
    [{
        remove_constraints = [SC_Typeclass selected.tc];
        add_constraints = deduced_single_line;
        add_constraints_simpl = cleaned :: deduced;
        proof_trace = Axiom (HandWaved "cut with (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
      }]

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)
    
let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppd "%a" c_typeclass_simpl_short t.tc

let printer_json (t : selector_output) =
  let open Type_variable_abstraction.Yojson in
  `Assoc [("tc",c_typeclass_simpl t.tc)]
let comparator ({ tc=a1 } : selector_output) ({ tc=b1 } : selector_output) =
  let open Type_variable_abstraction.Compare in
  c_typeclass_simpl a1 b1
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)

open Solver_types

module Compat = struct
  module All_plugins = Database_plugins.All_plugins.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
  include MM
  let compat_flds _flds : MM.flds =
    (module struct
    end)
  let selector repr c flds = MM.selector repr c (compat_flds flds)
  let alias_selector a b flds = MM.alias_selector a b (compat_flds flds)
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }
