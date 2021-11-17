open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "run"; "interpret" ; "yy"; "--init-file" ; (bad_test "linear_pattern_matching.mligo") ] ;
  [%expect {|
    File "../../test/contracts/negative/linear_pattern_matching.mligo", line 4, characters 34-35:
      3 | let yy : string = match { a = 1 ; b = 2n ; c = "33" } with
      4 |   | { a = a ;  b = b ; c = c } -> a

    Invalid type(s).
    Expected: "string", but got: "int". |}] ;

  run_ligo_good [ "run"; "interpret" ; "( (match (1,2n,\"3\") with | (a,b,c) -> a) : int )" ; "--syntax";"cameligo" ] ;
   [%expect {|
    1 |}] ;

  run_ligo_good [ "run"; "interpret" ; "match (1,2) with | (a,b) -> a" ; "--syntax";"cameligo" ] ;
   [%expect {|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret" ; "( (case (1,2n,\"3\") of (a,b,c) -> a end) : int)" ; "--syntax";"pascaligo" ] ;
   [%expect {|
    1 |}] ;

  run_ligo_good [ "run"; "interpret" ; "case (1,2) of (a,b) -> a end" ; "--syntax";"pascaligo" ] ;
   [%expect {|
    1 |}]

(* let%expect_test _ =
  run_ligo_good [ "interpret" ; "--syntax=reasonligo" ; "switch (1,2) { (a,b) => a }" ] ;
   [%expect {|
    1 |}] ; *)

  (* TODO: Syntax error #238 (?) not sure why *)
  (* run_ligo_good [ "interpret" ; "--syntax=reasonligo" ; "(switch (1,2,\"3\") { | (a,b,c) => a } : int)" ] ;
   [%expect {|
    1 ss|}] ; *)