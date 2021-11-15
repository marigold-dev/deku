open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/cycle_A.mligo",
      "child": {
        "file": "../../test/contracts/build/cycle_B.mligo",
        "child": {
          "file": "../../test/contracts/build/cycle_C.mligo",
          "child": {
            "file": "../../test/contracts/build/cycle_A.mligo",
            "child": { "file": "../../test/contracts/build/cycle_B.mligo" }
          }
        }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/D.mligo
        |-- ../../test/contracts/build/C.mligo
        |   |-- ../../test/contracts/build/A.mligo
        |   `-- ../../test/contracts/build/B.mligo
        |       `-- ../../test/contracts/build/A.mligo
        `-- ../../test/contracts/build/E.mligo
            |-- ../../test/contracts/build/F.mligo
            `-- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/D.mligo",
      "child": {
        "file": "../../test/contracts/build/C.mligo",
        "child": { "file": "../../test/contracts/build/A.mligo" },
        "child": {
          "file": "../../test/contracts/build/B.mligo",
          "child": { "file": "../../test/contracts/build/A.mligo" }
        }
      },
      "child": {
        "file": "../../test/contracts/build/E.mligo",
        "child": { "file": "../../test/contracts/build/F.mligo" },
        "child": { "file": "../../test/contracts/build/G.mligo" }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "B.mligo" ; "-e" ; "f" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { PUSH int 42 ;
             PUSH int 1 ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PUSH int 1 ;
             DIG 2 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "D.mligo" ] ;
  [%expect {|
    const toto = ADD(E.toto ,
    C.B.titi)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main = lambda (#6) return let #8 = #6 in  match #8 with
                                                     | ( p , s ) ->
                                                     let s = ADD(ADD(p , s) ,
                                                     toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
    let ../../test/contracts/build/A.mligo = let toto = L(1) in toto
    let ../../test/contracts/build/B.mligo =
      let A = ../../test/contracts/build/A.mligo[@inline] in
      let toto = L(32) in
      let titi = ADD(A , L(42)) in
      let f =
        fun #1 ->
        (let #4 = #1 in
         let (#10, #11) = #4 in
         let #2 = #10 in
         let x = #11 in let x = ADD(ADD(x , A) , titi) in PAIR(LIST_EMPTY() , x)) in
      PAIR(PAIR(A , f) , PAIR(titi , toto))
    let ../../test/contracts/build/F.mligo = let toto = L(44) in toto
    let ../../test/contracts/build/G.mligo = let toto = L(43) in toto
    let ../../test/contracts/build/C.mligo =
      let A = ../../test/contracts/build/A.mligo[@inline] in
      let B = ../../test/contracts/build/B.mligo[@inline] in
      let tata = ADD(A , CAR(CDR(B))) in
      let foo = (CDR(CAR(B)))@(PAIR(L(unit) , L(3))) in
      PAIR(PAIR(A , B) , PAIR(foo , tata))
    let ../../test/contracts/build/E.mligo =
      let F = ../../test/contracts/build/F.mligo[@inline] in
      let G = ../../test/contracts/build/G.mligo[@inline] in
      let toto = L(10) in
      let foo = L("bar") in PAIR(PAIR(F , G) , PAIR(foo , toto))
    let C = ../../test/contracts/build/C.mligo[@inline]
    let E = ../../test/contracts/build/E.mligo[@inline]
    let toto = ADD(CDR(CDR(E)) , CAR(CDR(CDR(CAR(C)))))
    let fb = (L(1), toto, L(2), L(3))
    let main =
      fun #6 ->
      (let #8 = #6 in
       let (#12, #13) = #8 in
       let p = #12 in
       let s = #13 in let s = ADD(ADD(p , s) , toto) in PAIR(LIST_EMPTY() , s)) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "D.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { PUSH int 42 ;
             PUSH int 1 ;
             ADD ;
             PUSH int 1 ;
             PAIR ;
             CDR ;
             PUSH int 10 ;
             ADD ;
             SWAP ;
             UNPAIR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "type_B.mligo" ] ;
  [%expect {|
    File "../../test/contracts/build/type_B.mligo", line 5, characters 5-6:
      4 | 	let s = s + 1 in
      5 | 	let p = p ^ "titi" in
      6 | 	([] : operation list), s
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter string ;
      storage int ;
      code { CDR ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ = 
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "tata" ; "--init-file" ; contract "C.mligo" ] ;
  [%expect {| 44 |}]

let%expect_test _ = 
  run_ligo_good [ "run" ; "test" ;  contract "C1.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; contract "C_test.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "Xmain.mligo" ] ;
  [%expect {|
    { 1 ; 2 ; 3 } |}]
    