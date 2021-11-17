open Trace
open Typer_common.Errors
open Database_plugins.All_plugins
open Ast_core.Types
open Solver_types

val heuristic : <
    assignments              : type_variable Assignments.t ;
    typeclasses_constraining : type_variable Typeclasses_constraining.t ;
  ..
> ex_heuristic_plugin

val restrict : raise:typer_error raise -> (type_variable -> type_variable) -> constructor_or_row -> c_typeclass_simpl -> c_typeclass_simpl

type deduce_and_clean_result = {
  deduced : constructor_or_row list ;
  cleaned : c_typeclass_simpl ;
  changed : bool ;
}
val deduce_and_clean : raise:typer_error raise -> (type_variable -> type_variable) -> c_typeclass_simpl -> deduce_and_clean_result

val pp_deduce_and_clean_result_short : Format.formatter -> deduce_and_clean_result -> unit
