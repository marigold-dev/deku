(* selector / propagation rule for accessing the type of a constructor or field
 * α = β.ℓ and β = κ() into α = τ where τ is c, b = d *)

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t Grouped_by_variable.t
  end
end

open Trace
open Typer_common.Errors

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction.Types
  open Type_variable_abstraction.Misc
  open Type_variable_abstraction.Reasons
  type type_variable = Type_variable.t

  type selector_output = {
    a_r_map : c_row_simpl ;
    a_var_l : c_access_label_simpl ;
  }

  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins

  open Type_variable_abstraction

  let heuristic_name = "break_ctor"

let printer ppf {a_r_map;a_var_l} =
  let open Type_variable_abstraction.PP in
  Format.fprintf ppf "{@[<hv 2> @ a_r_map : %a;@ a_var_l : %a;@]@ }"
    c_row_simpl_short a_r_map
    c_access_label_simpl_short a_var_l

let printer_json {a_r_map;a_var_l} =
  let open Type_variable_abstraction.Yojson in
  `Assoc [
    ("a_r_map", c_row_simpl a_r_map);
    ("a_var_l", c_access_label_simpl a_var_l)]
let comparator { a_r_map=a1; a_var_l=a2 } { a_r_map=b1; a_var_l=b2 } =
  let open Type_variable_abstraction.Compare in
  c_row_simpl a1 b1 <? fun () -> c_access_label_simpl a2 b2


let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun repr type_constraint_simpl (module Indexes) ->
  (* Format.eprintf "In access_label.selector for %a and indeces %a\n%!" Ast_core.PP.type_constraint_simpl_short type_constraint_simpl (GroupedByVariable.pp Ast_core.PP.type_variable) indexes#grouped_by_variable; *)
  match type_constraint_simpl with
    SC_Apply _ -> []
  | SC_Abs   _ -> []
  | SC_Constructor c -> (
      let other_access_labels_record_types = Grouped_by_variable.get_access_labels_by_record_type (repr c.tv) Indexes.grouped_by_variable in
      if MultiSet.is_empty other_access_labels_record_types then
        []
      else
        failwith (Format.asprintf "TODO: type error with %a ; %a" PP.c_constructor_simpl c (MultiSet.pp PP.c_access_label_simpl) other_access_labels_record_types)
    )
  | SC_Alias       _                -> []
  | SC_Typeclass   _                -> []
  | SC_Access_label l               -> (
      let other_rows_lhs = Grouped_by_variable.get_rows_by_lhs (repr l.record_type) Indexes.grouped_by_variable in
      let other_constructors_lhs = Grouped_by_variable.get_constructors_by_lhs (repr l.record_type) Indexes.grouped_by_variable in
      if not (MultiSet.is_empty other_constructors_lhs) then
        failwith (Format.asprintf "TODO: type error with %a (needs a record, but) %a (are constructors)" PP.c_access_label_simpl l (MultiSet.pp PP.c_constructor_simpl) other_constructors_lhs)
      else
      let cs_pairs = MultiSet.map_elements (fun x -> { a_r_map = x ; a_var_l = l }) other_rows_lhs in
      (* Format.eprintf "cs_pairs (%a)\n%!" (PP_helpers.list_sep_d printer) cs_pairs; *)
      cs_pairs
    )
  | SC_Poly        _                -> []
  | SC_Row         ({ r_tag = C_record ; _ } as r) -> (
      let other_access_labels_lhs = Grouped_by_variable.get_access_labels_by_record_type (repr r.tv) Indexes.grouped_by_variable in
      let cs_pairs = MultiSet.map_elements (fun x -> { a_r_map = r ; a_var_l = x }) other_access_labels_lhs in
      (* Format.eprintf "cs_pairs (%a)\n%!" (PP_helpers.list_sep_d printer) cs_pairs; *)
      cs_pairs
    )
  | SC_Row         _ -> []

let alias_selector_half : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_access_labels = Grouped_by_variable.get_access_labels_by_record_type a Indexes.grouped_by_variable in
  let b_rows = Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
  match MultiSet.elements b_rows with
  | [] -> []
  | old_rows_hd :: _ ->
    List.map ~f:(fun al -> {a_r_map = old_rows_hd; a_var_l = al}) (MultiSet.elements a_access_labels)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b indexes ->
  (* Format.eprintf "In access_label.alias_selector %a %a\n%!" Ast_core.PP.type_variable a Ast_core.PP.type_variable b ; *)
  alias_selector_half a b indexes @ alias_selector_half b a indexes

let get_referenced_constraints ({ a_r_map; a_var_l } : selector_output) : type_constraint_simpl list =
  [
    SC_Row a_r_map;
    SC_Access_label a_var_l;
  ]

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise selected repr ->
  (* Format.eprintf "In access_label.propagator for %a\n%!"
    printer selected; *)
  let a_var_l = selected.a_var_l in
  let a_r_map = selected.a_r_map in
  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  let row_tv = repr a_r_map.tv in
  let record_type = repr a_var_l.record_type in
  let access_result = repr a_var_l.tv in
  assert (Compare.type_variable row_tv record_type = 0);
  (* produce constraints: *)

  let field_type =
    match LMap.find_opt a_var_l.label a_r_map.tv_map with
    | None -> raise.raise @@ corner_case "Type error: label {a_var_l.label} does not exist in record {a_r_map.tv_map}"
    | Some field_type -> repr field_type.associated_variable
      
  in

  (* Produce constraint a_r_map.tv_map[label] = a_var_l.tv *)
  let eqs = [
    c_equation
      (wrap (Propagator_access_label "a") @@ P_variable access_result)
      (wrap (Propagator_access_label "b") @@ P_variable field_type)
      "propagator: break_ctor: row"
  ] in
  
  [
    {
      remove_constraints = [];
      add_constraints = eqs;
      add_constraints_simpl = [];
      proof_trace = Axiom (HandWaved "TODO: proof trace") (* Axiom Axioms.f_equal *)
    }
  ]

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
    a_r_map : c_row_simpl ;
    a_var_l : c_access_label_simpl ;
  }
