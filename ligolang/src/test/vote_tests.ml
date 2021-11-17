open Trace
open Test_helpers
open Main_errors

let get_program = get_program "./contracts/vote.mligo" (Contract "main")

open Ast_imperative

let init_storage name = e_record_ez [
    ("title" , e_string name) ;
    ("yea", e_nat 0) ;
    ("nay", e_nat 0) ;
    ("voters" , e_typed_set [] (t_address ())) ;
    ("start_time" , e_timestamp 0) ;
    ("finish_time" , e_timestamp 1000000000) ;
  ]

let reset title start_time finish_time =
  let reset_action = e_record_ez [
      ("title" , e_string title) ;
      ("start_time" , e_timestamp start_time) ;
      ("finish_time" , e_timestamp finish_time)]
  in e_constructor "Reset" reset_action

let yea = e_constructor "Vote" (e_constructor "Yea" (e_unit ()))

let init_vote ~raise ~add_warning () =
  let (program, env) = get_program ~raise ~add_warning () in
  let result =
    Test_helpers.run_typed_program_with_imperative_input ~raise
      (program, env) "main" (e_pair yea (init_storage "basic")) in
  let (_, storage) = trace_option ~raise (test_internal __LOC__) @@ Ast_core.extract_pair result in
  let storage' = trace_option ~raise (test_internal __LOC__) @@ Ast_core.extract_record storage in
  let storage' =  List.map ~f:(fun (Ast_core.Label l,v) -> (Label l, v)) storage' in
(*  let votes = List.assoc (Label "voters") storage' in
  let votes' = extract_map votes in *)
  let yea = List.Assoc.find_exn ~equal:Caml.(=) storage' (Label "yea") in
  let () = trace_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (yea, Ast_core.e_nat Z.one) in
  ()

let main = test_suite "Vote" [
    test_w "type" init_vote;
  ]
