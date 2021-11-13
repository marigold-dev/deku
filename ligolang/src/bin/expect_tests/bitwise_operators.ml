
open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n land 4n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n land 0n" ] ;
  [%expect{|
    0 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "7 land 4n" ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n lor 4n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n lor 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "7n lor 4n" ] ;
  [%expect{|
    7 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n lxor 4n" ] ;
  [%expect{|
    0 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n lxor 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "7n lxor 4n" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n lsl 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "7n lsl 1n" ] ;
  [%expect{|
    14 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "7n lsl 2n" ] ;
  [%expect{|
    28 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "4n lsr 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "14n lsr 1n" ] ;
  [%expect{|
    7 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "14n lsr 2n" ] ;
  [%expect{|
    3 |}]
  
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14 land 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat) or (int, nat), but got an argument of type int, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14n lor 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type nat, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14 lor 2n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14n lxor 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type nat, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14 lxor 2n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "4 lsr 0n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14n lsr 1" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type nat, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14 lsl 2n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "14n lsl 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type nat, int. |}]


let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n land 4n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n land 0n" ] ;
  [%expect{|
    0 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "7 land 4n" ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n lor 4n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n lor 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "7n lor 4n" ] ;
  [%expect{|
    7 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n lxor 4n" ] ;
  [%expect{|
    0 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n lxor 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "7n lxor 4n" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n lsl 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "7n lsl 1n" ] ;
  [%expect{|
    14 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "7n lsl 2n" ] ;
  [%expect{|
    28 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "4n lsr 0n" ] ;
  [%expect{|
    4 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "14n lsr 1n" ] ;
  [%expect{|
    7 |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "14n lsr 2n" ] ;
  [%expect{|
    3 |}]
  
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14 land 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat) or (int, nat), but got an argument of type int, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14n lor 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type nat, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14 lor 2n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14n lxor 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type nat, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14 lxor 2n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (bool, bool) or (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "4 lsr 0n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14n lsr 1" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type nat, int. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14 lsl 2n" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type int, nat. |}]
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "reasonligo" ; "14n lsl 2" ] ;
  [%expect{|
    Invalid arguments.
    Expected an argument of type (nat, nat), but got an argument of type nat, int. |}]