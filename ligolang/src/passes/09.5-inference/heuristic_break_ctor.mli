open Database_plugins.All_plugins
open Ast_core.Types
open Solver_types
open Typer_common.Errors

val heuristic : <
  grouped_by_variable : type_variable Grouped_by_variable.t ;
  ..
> ex_heuristic_plugin

type selector_output = {
  a_k_var : constructor_or_row ;
  a_k'_var' : constructor_or_row ;
}

val selector : ( type_variable -> type_variable ) -> type_constraint_simpl -> < grouped_by_variable : type_variable Grouped_by_variable.t > -> selector_output list
val propagator : (selector_output, typer_error) propagator
val comparator : selector_output -> selector_output -> int

(* module Indexes : functor (Type_variable : sig type t end) -> sig
 *   module type S = sig
 *     val grouped_by_variable : Type_variable.t GroupedByVariable.t
 *   end
 * end *)

(* module H : functor (Type_variable : sig type t end) -> sig
 *   open Typer_common.Errors
 *   type selector_output
 *   val heuristic_name : string
 *   val selector : (Type_variable.t -> type_variable) -> type_constraint_simpl ->
 *     (module Indexes(Type_variable).S)
 *     -> selector_output list
 *   val alias_selector : type_variable -> type_variable ->
 *     (module Indexes(Type_variable).S)
 *     -> selector_output list
 *   val get_referenced_constraints : selector_output -> type_constraint_simpl list
 *   val propagator : (selector_output, typer_error) propagator
 *   val printer : Format.formatter -> selector_output -> unit
 *   val printer_json : selector_output -> Yojson.Safe.t
 *   val comparator : selector_output -> selector_output -> int
 * end *)
