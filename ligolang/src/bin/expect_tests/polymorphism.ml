open Cli_expect

let test basename = "./" ^ basename
let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/polymorphism/"

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "zip(list [1;2;3], list [4n;5n;6n])" ; "--init-file" ; (test "comb.ligo") ] ;
  [%expect{|
    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "zip (zip(list [1;2;3], list [4n;5n;6n]), list [\"a\";\"b\";\"c\"])" ; "--init-file" ; (test "comb.ligo") ] ;
  [%expect{|
    { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "zip [1;2;3] [4n;5n;6n]" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{|
    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "zip (zip [1;2;3] [4n;5n;6n]) [\"a\";\"b\";\"c\"]" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{|
    { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "let (x, y) = diag 4 in x + y" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{|
    8 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{|
    { Pair "a" "a" ; Pair "b" "b" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "w" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{|
    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "(zip([1,2,3]))([4n,5n,6n])" ; "--init-file" ; (test "comb.religo") ] ;
  [%expect{|
    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "(zip((zip([1,2,3]))([4n,5n,6n])))([\"a\",\"b\",\"c\"])" ; "--init-file" ; (test "comb.religo") ] ;
  [%expect{|
    { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "jsligo" ; "(zip(list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]))" ; "--init-file" ; (test "comb.jsligo") ] ;
  [%expect{|
    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "jsligo" ; "(zip((zip(list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]))))(list([\"a\",\"b\",\"c\"]))" ; "--init-file" ; (test "comb.jsligo") ] ;
  [%expect{|
    { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "ctrct.mligo") ] ;
  [%expect{|
    { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; (test "test.mligo") ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "bar" ; "--init-file" ; (test "modules.mligo") ] ;
  [%expect{|
    (Pair (Some 1) (Some "hello")) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "bar" ; "--init-file" ; (test "modules.religo") ] ;
  [%expect{|
    (Pair (Some 1) (Some "hello")) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "foo" ; "--init-file" ; (test "use_nelist.mligo") ] ;
  [%expect{|
    { 2 ; 4 ; 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "bar" ; "--init-file" ; (test "use_nelist.mligo") ] ;
  [%expect{|
    12 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; (test "cases_annotation1.mligo") ] ;
  [%expect{|
    "hello" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; (test "cases_annotation2.mligo") ] ;
  [%expect{|
    "hello" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "solve 5" ; "--init-file" ; (test "use_monad.mligo") ] ;
  [%expect{|
    { Pair (Pair 3 4) 5 ; Pair (Pair 4 3) 5 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "solve 5" ; "--init-file" ; (test "use_monad_set.mligo") ] ;
  [%expect{|
    { Pair (Pair 3 4) 5 ; Pair (Pair 4 3) 5 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "auto" ; "solve(10)" ; "--init-file" ; (test "use_monad.jsligo") ] ;
  [%expect{|
    { Pair (Pair 3 4) 5 ; Pair (Pair 6 8) 10 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "map (fun (f : (string -> int -> int)) -> f \"hello\" 4) (uhms : (string -> int -> int) list)" ; "--init-file" ; (test "map.mligo") ] ;
  [%expect{|
    { 4 ; 4 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "bar 5" ; "--init-file" ; (test "use_error.mligo") ] ;
  [%expect{|
    1 |}]

let () = Sys.chdir pwd ;
         Sys.chdir "../../test/contracts/negative/polymorphism/"

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (test "annotate.mligo") ] ;
  [%expect{|
    File "./annotate.mligo", line 1, characters 0-26:
      1 | let f (type a) (x : a) = x

    Functions with type parameters need to be annotated. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (test "annotate2.mligo") ] ;
  [%expect{|
    File "./annotate2.mligo", line 1, characters 11-13:
      1 | let f (x : _a) = x

    Can't infer the type of this value, please add a type annotation. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (test "generalizable_type.mligo") ] ;
  [%expect{|
    File "./generalizable_type.mligo", line 1, characters 0-25:
      1 | type _foo = | Foo of unit
      2 |

    Invalid type name: _foo is a generalizable variable |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (test "constants.mligo") ] ;
  [%expect{|
    File "./constants.mligo", line 5, characters 8-13:
      4 |
      5 | let m = merge (Map.empty : (int, string) foo)

    These types are not matching:
     - string
     - int |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "f" ; "--init-file" ; (test "cases_annotation.mligo") ] ;
  [%expect{|
    File "./cases_annotation.mligo", line 4, characters 28-30:
      3 | let f (b : bool) (str : string) =
      4 |   let k = if b then k1 else k2 in
      5 |   k str (40 + 2)

    Can't infer the type of this value, please add a type annotation. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "bar 0" ; "--init-file" ; (test "use_error.mligo") ] ;
  [%expect{|
    An error occurred while evaluating an expression: Division by zero |}]


let () = Sys.chdir pwd
