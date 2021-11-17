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
open Simple_utils

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t  Grouped_by_variable.t
    val assignments : Type_variable.t Assignments.t
    val typeclasses_using_as_unbound_var : Type_variable.t Typeclasses_using_as_unbound_var.t
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
    c :  constructor_or_row ;
  }

  let heuristic_name = "tc_inline_named"
 
let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  let lst = t.tc in
  let a = t.c in fprintf ppd "%a and %a" c_typeclass_simpl_short lst constructor_or_row_short a

let printer_json (t : selector_output) =
  let open Type_variable_abstraction.Yojson in
  let lst = t.tc in
  let a = t.c in 
  `Assoc [
    ("tc",c_typeclass_simpl lst)
    ;("a",constructor_or_row a)]
let comparator { tc=a1; c=a2 } { tc=b1; c=b2 } =
  let open Type_variable_abstraction.Compare in
  c_typeclass_simpl a1 b1 <? fun () -> constructor_or_row a2 b2


(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* selector:
 *   find in db γᵢ = κ(β…)   and (…,αᵢ,…) ∈ ∃δ…, c… => [ (…,P_variable γᵢ,…) … ]
 *   find in db γᵢ = Ξ(ℓ:β…) and (…,αᵢ,…) ∈ ∃δ…, c… => [ (…,P_variable γᵢ,…) … ] *)
  
(* Find typeclass constraints in the dbs which constrain c_or_r.tv *)
let selector_by_variable : (type_variable -> type_variable) -> flds -> constructor_or_row -> type_variable -> selector_output list =
  fun repr (module Indexes) c_or_r tv ->
  let typeclasses = Typeclasses_using_as_unbound_var.get_list (repr tv) (module Indexes) in
  List.map ~f:(fun tc -> { tc ; c = c_or_r }) typeclasses

(* Find all constructor constraints γᵢ = κ(β …) and and row
   constraints γᵢ = Ξ(ℓ:β …) where γᵢ is one of the variables
   occurring as a root of one of the cells of the matrix of the
   typeclass. *)
let selector_by_tc : (type_variable -> type_variable) -> flds -> c_typeclass_simpl -> selector_output list =
  fun repr (module Indexes) tc ->
  let aux (tval : type_value) =
    match tval.wrap_content with
      P_variable tv ->
      (* Since we are only refining the typeclass one type expression
         node at a time, we only need the top-level assignment for
         that variable, e.g. α = κ(βᵢ, …). We can therefore look
         directly in the assignments. *)
      (match Assignments.find_opt (repr tv) Indexes.assignments with
       | Some cr -> [{ tc ; c = cr }]
       | None   -> [])
    | _ -> [] in
  List.concat @@ List.map ~f:aux (get_cells tc)

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun repr type_constraint_simpl indexes ->
  match type_constraint_simpl with
    SC_Apply _ -> []
  | SC_Abs   _ -> []
  | SC_Constructor c  -> selector_by_variable repr indexes (`Constructor c) c.tv
  | SC_Row r          -> selector_by_variable repr indexes (`Row         r) r.tv
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass   tc -> selector_by_tc repr indexes tc

(* When (αᵢ, …) ∈ { (τ, …) , … } and β = κ(δ …) are in the db,
   aliasing α and β should check if they are non-empty, and in that
   case produce a selector_output for all pairs. This will involve a
   lookup to see if α is constrained by a typeclass
   (typeclasses_constraining indexer). Add to this the logic for
   refined_typeclass vs. typeclass. *)

let alias_selector_half : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_tcs = Typeclasses_using_as_unbound_var.get_list a (module Indexes) in
  let b_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  let b_lhs_rows = Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
  let b_ctors = MultiSet.map_elements (fun a -> `Constructor a) b_lhs_constructors in
  let b_rows  = MultiSet.map_elements (fun a -> `Row a        ) b_lhs_rows         in
  List.concat @@
  List.map
    ~f:(fun tc ->
       List.map ~f:(fun c -> { tc ; c })
         (b_ctors @ b_rows ))
    a_tcs  

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b indexes ->
  alias_selector_half a b indexes @ alias_selector_half b a indexes

let get_referenced_constraints ({ tc; c } : selector_output) : type_constraint_simpl list =
  [
    SC_Typeclass tc;
    (match c with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let p_variable v = Location.wrap (* ~loc: TODO *) @@ P_variable v
let tv : constructor_or_row -> type_variable =
  function `Constructor c -> c.tv | `Row r -> r.tv
let row_value v = { associated_value = p_variable v.associated_variable; michelson_annotation = v.michelson_annotation ; decl_pos = v.decl_pos; }


let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise:_ selected repr ->
    (* Format.eprintf "in inline_named.propagator for %a\n%!"
    printer selected; *)
  let { tc; c } = selected in
  let inline_p_variable changed = function
      { Location.wrap_content = P_variable v ; location } when Compare.type_variable (repr v) (repr (tv c)) = 0 ->
      (* TODO: This is not quite the right location *)
      true, Location.wrap ~loc:location @@
      (match c with
         `Constructor { reason_constr_simpl; id_constructor_simpl=_; original_id=_; tv=_; c_tag; tv_list } ->
         let _ = reason_constr_simpl in (* TODO: use it *)
         P_constant { p_ctor_tag = c_tag ; p_ctor_args = (List.map ~f:p_variable tv_list) }
       | `Row { reason_row_simpl; id_row_simpl=_; original_id=_; tv=_; r_tag; tv_map } ->
         let _ = reason_row_simpl in (* TODO: use it *)
         P_row { p_row_tag = r_tag ; p_row_args = (LMap.map row_value tv_map) }
      )
    | other -> changed, other
  in
  let changed, updated_tc = fold_map_cells inline_p_variable false tc in
  if not changed then
    []
  else
    [{
      remove_constraints = [SC_Typeclass selected.tc];
      add_constraints = [];
      add_constraints_simpl = [SC_Typeclass updated_tc];
      proof_trace = Axiom (HandWaved "unfold")
    }]

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)
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
    let typeclasses_using_as_unbound_var : type_variable Typeclasses_using_as_unbound_var.t = flds#typeclasses_using_as_unbound_var
  end)
  let selector repr c flds = MM.selector repr c (compat_flds flds)
  let alias_selector a b flds = MM.alias_selector a b (compat_flds flds)
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }
