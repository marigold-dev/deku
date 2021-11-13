open Solver_types

module M = functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
struct
  open Type_variable_abstraction.Types
open Solver_types

type 'typeVariable t = (constraint_identifier, c_typeclass_simpl) PolyMap.t
let create_state ~cmp:_ =
  PolyMap.create ~cmp:Type_variable_abstraction.Compare.constraint_identifier

let add_constraint ?debug:_ _repr state new_constraint =
  match new_constraint with
  | SC_Typeclass c -> PolyMap.add c.id_typeclass_simpl c state
  | _ -> state
let remove_constraint ~raise:_ _ _repr state constraint_to_rm =
  match constraint_to_rm with
  | SC_Typeclass { id_typeclass_simpl; _ } ->
    (* TODO: a proper error instead of an exception *)
    if Ast_core.Debug.debug_new_typer then Format.eprintf "Remove from by Constraint Identifier...%!";
    let map = PolyMap.remove id_typeclass_simpl state in
    if Ast_core.Debug.debug_new_typer then Format.eprintf "  ok\n%!";
    map
  | _ -> state

let merge_aliases : 'old 'new_ . ?debug:(Format.formatter -> 'new_ t -> unit) -> ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun ?debug:_ _merge_keys state -> state

let pp _type_variable ppf state =
  let open PP_helpers in
  list_sep_d (pair Type_variable_abstraction.PP.constraint_identifier Type_variable_abstraction.PP.c_typeclass_simpl_short) ppf (PolyMap.bindings state)

let name = "by_constraint_identifier"

let find_opt : constraint_identifier -> 'type_variable t -> c_typeclass_simpl option = PolyMap.find_opt

let get_state_for_tests state = state
end
