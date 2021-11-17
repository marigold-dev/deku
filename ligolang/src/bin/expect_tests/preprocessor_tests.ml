open Cli_expect

let%expect_test _ =
  run_ligo_bad ["compile" ; "contract"; "../../test/preprocessor/directive_inside_line.ligo"];
  [%expect {|
File "../../test/preprocessor/directive_inside_line.ligo", line 1, characters 2-10:
  1 |   #include "foo"
File "../../test/preprocessor/directive_inside_line.ligo", line 1, characters 2-10:
Directive inside a line.
 |}];
