open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.ligo" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.ligo", line 6, characters 21-27:
      5 |
      6 | function main (const action : unit; const store : storage) : return is block {
      7 |   const foo : storage = ("foo",(1,2n)) ;
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/michelson_pair_tree.ligo", line 6, characters 42-47:
      5 |
      6 | function main (const action : unit; const store : storage) : return is block {
      7 |   const foo : storage = ("foo",(1,2n)) ;
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter unit ;
      storage (pair (string %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH string "foo" ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.mligo" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.mligo", line 6, characters 10-16:
      5 |
      6 | let main (action, store : unit * storage) : return =
      7 |   let foo = (3,(1,2n)) in
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/michelson_pair_tree.mligo", line 6, characters 18-23:
      5 |
      6 | let main (action, store : unit * storage) : return =
      7 |   let foo = (3,(1,2n)) in
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.religo" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.religo", line 6, characters 13-19:
      5 |
      6 | let main = ((action, store) : (unit , storage)) : return => {
      7 |   let foo = (3,(1,2n)) ;
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/michelson_pair_tree.religo", line 6, characters 21-26:
      5 |
      6 | let main = ((action, store) : (unit , storage)) : return => {
      7 |   let foo = (3,(1,2n)) ;
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.jsligo", line 8, characters 21-26:
      7 |
      8 | let main = ([action, store] : [unit, storage]) : return_ => {
      9 |   let foo = [3, [1, 2 as nat]];
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/michelson_pair_tree.jsligo", line 8, characters 13-19:
      7 |
      8 | let main = ([action, store] : [unit, storage]) : return_ => {
      9 |   let foo = [3, [1, 2 as nat]];
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree_intermediary.ligo" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree_intermediary.ligo", line 6, characters 21-27:
      5 |
      6 | function main (const action : unit; const store : storage) : return is block {
      7 |   const foo : storage = ("foo",(1,2n)) ;
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/michelson_pair_tree_intermediary.ligo", line 6, characters 42-47:
      5 |
      6 | function main (const action : unit; const store : storage) : return is block {
      7 |   const foo : storage = ("foo",(1,2n)) ;
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter unit ;
      storage (pair (string %three) (pair (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH string "foo" ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]
