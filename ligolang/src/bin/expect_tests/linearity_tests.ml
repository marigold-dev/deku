open Cli_expect

let c = "../../test/contracts/negative/linearity.mligo"
let p = "../../test/contracts/negative/linearity.ligo"


let%expect_test _ =
  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file" ; c ] ;
  [%expect {|
    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file"; p ] ;
  [%expect {|
    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "let foo (x, x : int * int) : int = x in foo 1"  ; "--syntax";"cameligo" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "let bar (p : int * int) : int = let (x, x) = p in x in bar (1,2)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}];

  (* run_ligo_bad [ "interpret" ; "--syntax=cameligo" ; "(( (x,x) : (int , int) ) : int => x) (1,1)" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}]; *)