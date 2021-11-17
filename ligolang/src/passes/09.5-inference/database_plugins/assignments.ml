open Solver_types

module M = functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
struct
  open Type_variable_abstraction.Types
open UnionFind

(* Haskell doesn't have easy-to-use type-level functions or types as
   fields of records, so we're bending its syntax here.

   type "Assignments.t" typeVariable = map typeVariable
   c_constructor_simpl data Assignments :: Plugin "Assignments.t" *)

type 'typeVariable t = ('typeVariable, constructor_or_row) ReprMap.t
let create_state ~cmp =
  (* we only keep one assigment becaucas they will all be equal *)
  let merge _c1 c2 = c2 in
  ReprMap.create ~cmp ~merge

(** Stores the first assignment ('a = ctor('b, …)) that is encountered
    (all assignments should use compatible types).

    Subsequent ('a = ctor('b2, …)) with the same 'a are ignored. *)
let add_constraint ?debug repr state new_constraint =
  let debug = match debug with
    Some (debug) -> Some (fun ppf (a,_) -> debug ppf a)
  | None -> None in
  match new_constraint with
  | SC_Constructor c ->
    Option.value ~default:state @@ ReprMap.add_opt ?debug (repr c.tv) (`Constructor c) state
  | SC_Row r ->
    Option.value ~default:state @@ ReprMap.add_opt ?debug (repr r.tv) (`Row r) state
  | _ -> state

let remove_constraint ~raise:_ _ _repr state _constraint_to_remove =
  (* assignments cannot be remove (they are similar to instanciations
     of existential variables in Coq, and happen globally regardless
     of the constraints available in the database). *)
  state

let merge_aliases : 'old 'new_ . ?debug:(Format.formatter -> 'new_ t -> unit) -> ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun ?debug:_ merge_keys state -> merge_keys.map state

let pp : 'a . (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit = fun _type_variable ppf state ->
  let open PP_helpers in
  Format.fprintf ppf "@[ %a @]"
    (list_sep_d
       (fun ppf (_,v) ->
          Format.fprintf ppf "%a" Type_variable_abstraction.PP.constructor_or_row_short v))
    (ReprMap.bindings state)

                                                                                                                             

let name = "assingments"

let find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option = ReprMap.find_opt

let bindings : 'type_variable t -> ('type_variable * constructor_or_row) list = fun state -> ReprMap.bindings state

end
