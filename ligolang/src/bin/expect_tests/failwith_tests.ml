open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good ["run"; "evaluate-call" ; contract "failwith.ligo"; "1" ; "-e"; "failer"; "--warn";"false"] ;
  [%expect {|
    failwith(42) |}];

  run_ligo_good ["run"; "evaluate-call" ; contract "failwith.ligo" ; "1" ; "-e" ; "failer" ; "--format";"json" ; "--warn";"false" ] ;
  [%expect {|
    { "value": null, "failure": "failwith(42)" } |}];


  run_ligo_good ["run"; "dry-run" ; contract "subtle_nontail_fail.mligo" ; "()" ; "()" ] ;
  [%expect {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 9-27:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    failwith("This contract always fails") |}];

  run_ligo_good ["run"; "interpret" ; "assert(1=1)" ; "--syntax";"pascaligo" ] ;
  [%expect {|
    unit |}];

  run_ligo_good ["run"; "interpret" ; "assert(1=2)" ; "--syntax";"pascaligo" ] ;
  [%expect {|
    failwith("failed assertion") |}];

  run_ligo_good ["run"; "interpret" ; "assert(1=1)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    unit |}];

  run_ligo_good ["run"; "interpret" ; "assert(1=2)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    failwith("failed assertion") |}];
