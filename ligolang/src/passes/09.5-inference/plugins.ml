open Solver_types

module Indexers = Database_plugins
let heuristics : Indexers.Indexers_plugins_fields(PerPluginState).flds heuristic_plugins = [
  Heuristic_break_ctor.heuristic ;
  Heuristic_access_label.heuristic ;
  Heuristic_specialize1.heuristic ;
  Heuristic_tc_fundep.heuristic ;
  Heuristic_tc_inline_named.heuristic ;
  Heuristic_tc_deduce_and_clean.heuristic ;
  Heuristic_tc_beta_p_apply.heuristic ;
]

(* module HBC = Heuristic_break_ctor.H *)
