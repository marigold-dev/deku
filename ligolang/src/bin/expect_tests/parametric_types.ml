open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1" ; "--init-file" ; (test "parametric_types.mligo") ] ;
  [%expect{|
    ( 1 , "one" ) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2" ; "--init-file" ; (test "parametric_types.mligo") ] ;
  [%expect{|
    CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3" ; "--init-file" ; (test "parametric_types.mligo") ] ;
  [%expect{|
    ( +1 , 1 ) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4" ; "--init-file" ; (test "parametric_types.mligo") ] ;
  [%expect{|
    CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY()))) |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1" ; "--init-file" ; (test "parametric_types.religo") ] ;
  [%expect{|
    ( 1 , "one" ) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2" ; "--init-file" ; (test "parametric_types.religo") ] ;
  [%expect{|
    CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3" ; "--init-file" ; (test "parametric_types.religo") ] ;
  [%expect{|
    ( +1 , 1 ) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4" ; "--init-file" ; (test "parametric_types.religo") ] ;
  [%expect{|
    CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY()))) |}]
  
(* let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(test "parametric_types.jsligo") ; "t1" ] ;
  [%expect{|
    ( 1 , "one" ) |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(test "parametric_types.jsligo") ; "t2" ] ;
  [%expect{|
    CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))) |}] *)

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1" ; "--init-file" ; (test "parametric_types.ligo") ] ;
  [%expect{|
    ( 1 , "one" ) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2" ; "--init-file" ; (test "parametric_types.ligo") ] ;
  [%expect{|
    CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3" ; "--init-file" ; (test "parametric_types.ligo") ] ;
  [%expect{|
    ( +1 , 1 ) |}]
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4" ; "--init-file" ; (test "parametric_types.ligo") ] ;
  [%expect{|
    CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY()))) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types1.mligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types1.mligo", line 1, characters 20-28:
      1 | type fail_big_map = bool map

    Type map takes the wrong number of arguments, expected: 2 got: 1 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types2.mligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types2.mligo", line 2, characters 11-27:
      1 | type 'a foo = 'a * 'a
      2 | type bar = (int,string) foo

    Type foo takes the wrong number of arguments, expected: 1 got: 2 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types3.mligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types3.mligo", line 2, characters 11-18:
      1 | type ('a,'b,'c) foo = 'a * 'b * 'c
      2 | type bar = int foo

    Type foo takes the wrong number of arguments, expected: 3 got: 1 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types4.mligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types4.mligo", line 1, characters 9-15:
      1 | type x = option list

    Type takes the wrong number of arguments, expected: 1 got: 0 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types5.mligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types5.mligo", line 1, characters 9-11:
      1 | type ('a,'a) foo = 'a * 'a

    Repeated type variable "a" in type declaration.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types1.ligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types1.ligo", line 1, characters 21-30:
      1 | type fail_big_map is map(bool)

    Type map takes the wrong number of arguments, expected: 2 got: 1 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types2.ligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types2.ligo", line 2, characters 12-27:
      1 | type foo(a) is a * a
      2 | type bar is foo(int,string)

    Type foo takes the wrong number of arguments, expected: 1 got: 2 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types3.ligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types3.ligo", line 2, characters 12-20:
      1 | type foo(a,b,c) is a * b * c
      2 | type bar is foo(int)

    Type foo takes the wrong number of arguments, expected: 3 got: 1 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types4.ligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types4.ligo", line 1, characters 15-21:
      1 | type x is list(option)

    Type takes the wrong number of arguments, expected: 1 got: 0 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types5.ligo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types5.ligo", line 1, characters 11-12:
      1 | type foo(a,a) is a * a

    Repeated type variable "a" in type declaration.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types1.religo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types1.religo", line 1, characters 20-29:
      1 | type fail_big_map = map(bool);

    Type map takes the wrong number of arguments, expected: 2 got: 1 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types2.religo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types2.religo", line 2, characters 11-27:
      1 | type foo('a) = ('a , 'a);
      2 | type bar = foo (int,string);

    Type foo takes the wrong number of arguments, expected: 1 got: 2 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types3.religo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types3.religo", line 2, characters 11-19:
      1 | type foo('a,'b,'c) = ('a , 'b , 'c);
      2 | type bar = foo(int);

    Type foo takes the wrong number of arguments, expected: 3 got: 1 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types4.religo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types4.religo", line 1, characters 14-20:
      1 | type x = list(option);

    Type takes the wrong number of arguments, expected: 1 got: 0 |}]
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "parametric_types5.religo")] ;
  [%expect{|
    File "../../test/contracts/negative/parametric_types5.religo", line 1, characters 12-14:
      1 | type foo('a,'a) = ('a , 'a);

    Repeated type variable "a" in type declaration.
    Hint: Change the name. |}]
