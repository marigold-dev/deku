[@@@warning "-32"]
module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

(* open Trace
 * open Typer_common.Errors *)
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

module INDEXES_ = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val by_constraint_identifier : Type_variable.t By_constraint_identifier.t
  end
end

module Utils = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  include Heuristic_tc_utils.Utils(Type_variable)(Type_variable_abstraction)
  open All_plugins

  type flds_ = (module INDEXES_(Type_variable)(Type_variable_abstraction).S)

  let set_of_vars l = (Set.add_list l (Set.create ~cmp:Compare.type_variable )).set
  
  let constraint_identifier_to_tc ((module Indexes) : flds_) (ci : constraint_identifier) =
    (* TODO: this can fail: catch the exception and throw an error *)
    match By_constraint_identifier.find_opt ci Indexes.by_constraint_identifier with
      Some x -> x
    | None -> failwith "TODO: internal error : do something"

  let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
    fun tc -> tc.id_typeclass_simpl

  type 'v constraint_identifierMap = (constraint_identifier, 'v) RedBlackTrees.PolyMap.t
  (* type refined_typeclass_constraint_identifierMap = refined_typeclass constraint_identifierMap *)
  (* type constraint_identifier_set = constraint_identifier Set.t
  * type constraint_identifier_set_map = constraint_identifier_set typeVariableMap *)

  type private_storage = unit
  (* type private_storage = {
  *   (* This rule maintains in its private storage a representation of
  *      the latest version of each typeclass *)
  *   refined_typeclasses: refined_typeclass_constraint_identifierMap ;
  *   typeclasses_constrained_by : constraint_identifier_set_map ;
  * } *)

  let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
    fun tc -> tc.id_typeclass_simpl

  let splice f idx l =
    let rec splice acc f idx l =
      match l with
        [] -> failwith "invalid index into list"
      | hd :: tl ->
        if idx = 0
        then (List.append (List.rev acc) @@ List.append (f hd) @@ tl)
        else (splice (hd :: acc) f (idx - 1) tl)
    in splice [] f idx l

  let splice_or_none f idx l =
    let rec splice_or_none acc f idx l =
      match l with
        [] -> failwith "internal error: invalid index into list"
      | hd :: tl ->
        if idx = 0
        then (match f hd with
            | None -> None
            | Some new_hds -> Some (List.append (List.rev acc) @@ List.append new_hds @@ tl))
        else (splice_or_none (hd :: acc) f (idx - 1) tl)
    in splice_or_none [] f idx l
end
