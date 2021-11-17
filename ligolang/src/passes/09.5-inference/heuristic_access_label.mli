open Database_plugins.All_plugins
open Ast_core.Types
open Solver_types

type selector_output = {
  a_r_map : c_row_simpl ;
  a_var_l : c_access_label_simpl ;
}

val heuristic : <
  grouped_by_variable : type_variable Grouped_by_variable.t ;
  ..
> ex_heuristic_plugin
