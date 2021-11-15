open Inference.Compare_renaming
open Inference.Heuristic_tc_fundep

let deduce_and_clean_result : deduce_and_clean_result cmp  = fun expected actual ->
  let { deduced = a1 ; cleaned = a2 } = expected in
  let { deduced = b1 ; cleaned = b2 } = actual in
  constructor_or_row_list a1 b1 <? fun () -> c_typeclass_simpl a2 b2

let compare_and_check_vars_deduce_and_clean_result expected actual =
  compare_and_check_vars
    ~compare:deduce_and_clean_result
    ~print_whole:pp_deduce_and_clean_result_short
    expected
    actual
