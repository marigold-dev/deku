open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ a#0 f#5 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
[ a#0 f#5 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 4-5
[ a#0 f#5 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
[ a#0 g#3 i#1 j#2 k#4 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
[ a#0 g#3 i#1 j#2 k#4 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
[ a#0 g#3 i#1 j#2 k#4 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13
[ a#0 g#3 i#1 j#2 k#4 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
[ a#0 g#3 i#1 j#2 k#4 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
[ a#0 g#3 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25
[ a#0 g#3 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21
[ a#0 g#3 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17
[ a#0 g#3 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13
[ a#0 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21
[ a#0 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17
[ a#0 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13
[ ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#6 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#5 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#3 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#1 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 36-45 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#2 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 46-55 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#4 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
[ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
[ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
[ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
[ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
[ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9
[ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5
[ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21
[ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17
[ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13
[ a#0 c#1 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17
[ a#0 c#1 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13
[ a#0 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15
[ a#0 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-11
[ ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#5 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#1 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#4 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#2 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#3 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ a#0 f#3 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 6-7
[ a#0 f#3 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 4-5
[ a#0 f#3 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
[ a#0 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
[ a#0 i#1 j#2 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
[ ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#4 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#3 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#1 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 35-44 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#2 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 45-54 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#1 b#5 c#9 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    [ a#1 b#5 c#9 mytype#0 s#11 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21
    [ a#1 b#5 c#9 mytype#0 s#11 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    [ a#1 b#5 c#9 d#10 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    [ a#1 b#5 c#9 d#10 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29
    [ a#1 b#5 c#9 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 9-32
    [ a#1 b#5 hd#8 mytype#0 tl#7 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ a#1 b#5 c#6 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ a#1 b#5 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 13, character 4 to line 14, character 5
    [ a#1 b#5 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 18-19
    [ a#1 b#5 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 15-16
    [ a#1 b#5 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 11-12
    [ a#1 mytype#0 y#4 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18
    [ a#1 mytype#0 y#4 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-14
    [ a#1 mytype#0 x#3 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18
    [ a#1 mytype#0 x#3 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    [ a#1 c#2 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ a#1 mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ mytype#0 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9

    Variable definitions:
    (a#1 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#5 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#2 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#6 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#9 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#10 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#12 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#8 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#11 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#7 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#3 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#4 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#0 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#0 b#6 c#5 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    [ a#0 b#6 c#5 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    [ a#0 b#6 c#5 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    [ a#0 c#5 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ a#0 c#1 i#2 j#3 k#4 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 9-10
    [ a#0 c#1 i#2 j#3 k#4 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    [ a#0 c#1 i#2 j#3 k#4 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    [ a#0 c#1 i#2 j#3 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21
    [ a#0 c#1 i#2 j#3 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    [ a#0 c#1 i#2 j#3 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    [ a#0 c#1 ]
    [ ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9

    Variable definitions:
    (a#0 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#6 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#7 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#1 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#5 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#2 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#3 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#4 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    [ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    [ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    [ a#3 c#1 e#2 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    [ a#3 c#1 e#2 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9
    [ a#3 c#1 e#2 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    [ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21
    [ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17
    [ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13
    [ a#0 c#1 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17
    [ a#0 c#1 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13
    [ a#0 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15
    [ a#0 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-11
    [ ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9

    Variable definitions:
    (a#0 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#3 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#5 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#1 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#4 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#2 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#1 b#4 g#5 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    [ a#1 b#4 g#5 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    [ a#1 b#4 g#5 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    [ a#1 b#4 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 15-41
    [ a#1 b#4 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4
    [ a#1 i#2 j#3 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    [ a#1 i#2 j#3 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    [ a#1 i#2 j#3 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41
    [ a#1 i#2 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ a#1 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-45
    [ a#1 myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 53-55
    [ myrec#0 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9

    Variable definitions:
    (a#1 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#4 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#6 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#5 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#2 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#3 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#0 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions: |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--syntax" ; "cameligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#0 e#3 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    [ a#0 e#3 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    [ a#0 e#3 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23
    [ a#0 e#3 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21
    [ a#0 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ a#0 c#1 d#2 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44
    [ a#0 c#1 d#2 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    [ a#0 c#1 d#2 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    [ a#0 c#1 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9

    Variable definitions:
    (a#0 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#4 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#1 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 9-18 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#2 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#3 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions: |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--syntax";"cameligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ c#4 f#2 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ f#2 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 22-36
    [ b#3 f#2 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 18-19
    [ b#3 f#2 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    [ f#2 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-19
    [ i#0 j#1 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    [ i#0 j#1 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59

    Variable definitions:
    (a#5 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#3 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#4 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#2 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#0 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 35-44 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#1 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 45-54 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#0 b#5 x#6 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 12-13
    [ a#0 b#5 x#6 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    [ a#0 b#5 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    [ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    [ a#0 c#1 d#4 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    [ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    [ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    [ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9
    [ a#0 c#1 e#2 f#3 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5
    [ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21
    [ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17
    [ a#0 c#1 e#2 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13
    [ a#0 c#1 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17
    [ a#0 c#1 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13
    [ a#0 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15
    [ a#0 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-11
    [ ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9

    Variable definitions:
    (a#0 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#5 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#1 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#4 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#2 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#3 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#6 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#7 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#3 c#1 foo_record#0 j#4 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ a#3 c#1 foo_record#0 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ c#1 foo_record#0 i#2 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ c#1 foo_record#0 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ foo_record#0 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, characters 8-9
    [ foo_record#0 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 5, characters 8-9

    Variable definitions:
    (a#3 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#5 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#1 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#2 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#4 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#0 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions: |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#2 b#3 c#4 foo_record#1 foo_variant#0 p#5 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ a#2 b#3 foo_record#1 foo_variant#0 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    [ a#2 b#3 foo_record#1 foo_variant#0 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    [ a#2 foo_record#1 foo_variant#0 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ foo_record#1 foo_variant#0 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13

    Variable definitions:
    (a#2 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#3 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#4 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#6 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#5 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 9-25 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#1 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#0 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions: |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ a#3 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 6-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#1 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (a#3 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 12-13 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 6-7
    (b#2 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#4 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    (toto#0 -> toto) File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 8-12 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 10-14 ,
      File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 10-14
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3 :
    Variable definitions:
    (toto#0 -> toto) File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 8-12 |resolved: int|
    references: []
    Type definitions:
    Module definitions:

    (B -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3 :
    Variable definitions:
    (toto#0 -> toto) File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 8-12 |resolved: int|
    references: []
    Type definitions:
    Module definitions:

    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7 :
    Variable definitions:
    (a#3 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 12-13 |resolved: int|
    references: []
    Type definitions:
    Module definitions:

    (D -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7 :
    Variable definitions:
    (a#3 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 12-13 |resolved: int|
    references: []
    Type definitions:
    Module definitions: |} ]