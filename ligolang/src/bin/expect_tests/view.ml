open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

(* not warning is expected because the annotated view is still being included in the contract *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ; "--views" ; "v1,v2" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* the following should trigger a warning because an annotated view is being ignored *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ; "--views" ; "v2" ] ;
  [%expect {|
    File "../../test/contracts/view.mligo", line 3, characters 12-14:
      2 |
      3 | [@view] let v1 (n,s: int * int) : int = s + n + 1
      4 | let v2 (_,s: int * int) : int = s + 2

    This view will be ignored, command line option override [
    view] annotation

    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* bad view type *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ; "--views" ; "v1,bad_view" ] ;
  [%expect {|
    File "../../test/contracts/view.mligo", line 5, characters 13-30:
      4 | let v2 (_,s: int * int) : int = s + 2
      5 | let bad_view (_,_: int * nat ) : nat = 1n
      6 |

    Invalid view argument.
    View 'bad_view' has storage type 'nat' and contract 'main' has storage type 'int'. |}]