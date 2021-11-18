open Cli_expect


let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "change_color_preference(acc, Green)" ; "--init-file" ; (test "record.ligo") ] ;
  [%expect{| record[id -> 1 , preferences -> record[color -> Green(unit) , other -> 1]] |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "change_color_preference acc Green" ; "--init-file" ; (test "record.mligo") ] ;
  [%expect {|
    record[id -> 1 , preferences -> record[color -> Green(unit) , other -> 1]] |}]