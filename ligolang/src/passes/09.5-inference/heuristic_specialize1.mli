open Database_plugins.All_plugins
open Ast_core.Types
open Solver_types
open Typer_common.Errors

val heuristic : <
  grouped_by_variable : type_variable Grouped_by_variable.t ;
  ..
> ex_heuristic_plugin

type selector_output = {
    poly : c_poly_simpl ;
    a_k_var : c_constructor_simpl ;
  }

val selector : ( type_variable -> type_variable ) -> type_constraint_simpl -> < grouped_by_variable : type_variable Grouped_by_variable.t > -> selector_output list
val propagator : (selector_output, typer_error) propagator
val comparator : selector_output -> selector_output -> int
