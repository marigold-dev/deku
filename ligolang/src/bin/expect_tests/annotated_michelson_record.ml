open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

(*COMB*)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "annotated_michelson_record_comb.mligo" ; "--entry-point" ; "main_comb_two" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             { parameter unit ;
               storage (pair (int %anbfoo) (string %anabar)) ;
               code { CDR ; DUP ; CAR ; UPDATE 1 ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile" ; "contract" ; contract "annotated_michelson_record_comb.mligo" ; "--entry-point" ; "main_comb_three" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             { parameter unit ;
               storage (pair (int %ana) (pair (string %anb) (nat %anc))) ;
               code { DROP ;
                      PUSH int 1 ;
                      PUSH string "" ;
                      PUSH nat 1 ;
                      SWAP ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile" ; "contract" ; contract "annotated_michelson_record_comb.mligo" ; "--entry-point" ; "main_comb_five" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             { parameter unit ;
               storage
                 (pair (int %an_One)
                       (pair (string %an_Two) (pair (bool %an_Three) (pair (nat %an_Four) (int %an_Five))))) ;
               code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "dry-run" ; contract "annotated_michelson_record_comb.mligo" ; "()" ; "{ foo = 2 ; bar = \"bar\" }" ; "-e" ; "main_comb_two" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             ( LIST_EMPTY() , record[bar -> "bar" , foo -> 2] ) |}];
  run_ligo_good [ "run" ; "dry-run" ; contract "annotated_michelson_record_comb.mligo" ; "()" ; "{ a = 2 ; b = \"\" ; c = 1n }" ; "-e" ; "main_comb_three" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             ( LIST_EMPTY() , record[a -> 1 , b -> "" , c -> +1] ) |}];
  run_ligo_good [ "run" ; "dry-run" ; contract "annotated_michelson_record_comb.mligo" ; "()" ; "{ one = 1 ; two = \"\" ; three = true ; four = 2n ; five = 1 }" ; "-e" ; "main_comb_five" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_comb.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             ( LIST_EMPTY() ,
               record[five -> 1 , four -> +2 , one -> 1 , three -> True(unit) , two -> ""] ) |}]

(*TREE*)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "annotated_michelson_record_tree.mligo" ; "--entry-point" ; "main_comb_two" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             { parameter unit ;
               storage (pair (string %anbar) (int %anfoo)) ;
               code { CDR ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile" ; "contract" ; contract "annotated_michelson_record_tree.mligo" ; "--entry-point" ; "main_comb_three" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             { parameter unit ;
               storage (pair (pair (int %ana) (string %anb)) (nat %anc)) ;
               code { DROP ;
                      PUSH nat 1 ;
                      PUSH string "" ;
                      PUSH int 1 ;
                      PAIR ;
                      PAIR ;
                      NIL operation ;
                      PAIR } } |}];
  run_ligo_good [ "compile" ; "contract" ; contract "annotated_michelson_record_tree.mligo" ; "--entry-point" ; "main_comb_five" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             { parameter unit ;
               storage
                 (pair (pair (pair (int %an_Five) (nat %an_Four)) (pair (int %an_One) (bool %an_Three)))
                       (string %an_Two)) ;
               code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "dry-run" ; contract "annotated_michelson_record_tree.mligo" ; "()" ; "{ foo = 2 ; bar = \"bar\" }" ; "-e" ; "main_comb_two"  ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             ( LIST_EMPTY() , record[bar -> "bar" , foo -> 2] ) |}];
  run_ligo_good [ "run" ; "dry-run" ; contract "annotated_michelson_record_tree.mligo" ; "()" ; "{ a = 2 ; b = \"\" ; c = 1n }" ; "-e" ; "main_comb_three" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             ( LIST_EMPTY() , record[a -> 1 , b -> "" , c -> +1] ) |}];
  run_ligo_good [ "run" ; "dry-run" ; contract "annotated_michelson_record_tree.mligo" ; "()" ; "{ one = 1 ; two = \"\" ; three = true ; four = 2n ; five = 1 }" ; "-e" ; "main_comb_five" ] ;
  [%expect {|
             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 41, characters 20-26:
              40 |
              41 | let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
              42 |   ([] : operation list), store
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 21-27:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 38, characters 29-34:
              37 |
              38 | let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
              39 |   ([] : operation list), { a = 1 ; b = "" ; c = 1n }
             :
             Warning: unused variable "store".
             Hint: replace it by "_store" to prevent this warning.

             File "../../test/contracts/annotated_michelson_record_tree.mligo", line 33, characters 19-25:
              32 |
              33 | let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
              34 |   let o = store.foo in
             :
             Warning: unused variable "action".
             Hint: replace it by "_action" to prevent this warning.

             ( LIST_EMPTY() ,
               record[five -> 1 , four -> +2 , one -> 1 , three -> True(unit) , two -> ""] ) |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "accesses " ; "--init-file" ; (contract "annotated_michelson_record_comb.mligo") ] ;
  [%expect {|
             ( 1 , "" , True(unit) , +1 , 2 ) |}]