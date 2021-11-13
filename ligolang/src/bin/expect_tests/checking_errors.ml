open Cli_expect

(* Record *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "cameligo" ; "type foo = { foo : int } in let a : foo = {foo = 1n} in a" ] ;
  [%expect {|
             Invalid type(s).
             Expected: "record[foo -> int]", but got: "record[foo -> nat]".
             |}]

(* Record_update *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "cameligo" ; "let a = {foo = 1} in { a with foo = 1n}" ; "--infer" ] ;
  [%expect {|
             An internal error ocurred. Please, contact the developers.
             type error: incompatible types, not same ctor {
                                                              id_constructor_simpl : 0;
                                                              original_id : null;
                                                              reason_constr_simpl : simplifier: split constant literal_nat#5 = C_nat ();
                                                              tv : literal_nat#5;
                                                              c_tag : C_nat;
                                                              tv_list : ;
             } vs. {
                      id_constructor_simpl : 2;
                      original_id : null;
                      reason_constr_simpl : simplifier: split constant literal_int#1 = C_int ();
                      tv : literal_int#1;
                      c_tag : C_int;
                      tv_list : ;
             } (compare returns -1).
             |}]

(* Constructor *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "cameligo" ; "type foo = Foo of int in let a = Foo (5n) in a" ] ;
  [%expect {|
             Invalid type(s).
             Expected: "int", but got: "nat".
             |}]
