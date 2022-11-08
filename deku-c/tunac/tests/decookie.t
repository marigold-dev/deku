  $ ../bin/tunacc_test_operation.exe originate decookie.tz "map {}"
  Uncaught exception:
    
    (Invalid_argument "result is Error _")
  
  Raised at Stdlib.invalid_arg in file "stdlib.ml" (inlined), line 30, characters 20-45
  Called from Stdlib__Result.get_ok in file "result.ml", line 21, characters 45-76
  Called from Dune__exe__Tunacc_test_operation.originate in file "deku-c/tunac/bin/tunacc_test_operation.ml", line 18, characters 4-53
  Called from Core__Command.For_unix.run.(fun) in file "core/src/command.ml", line 2871, characters 8-270
  Called from Base__Exn.handle_uncaught_aux in file "src/exn.ml", line 127, characters 6-10
  [1]
  $ ../bin/tunacc_test_operation.exe invoke "DK1NmndDdhkWdWpX7NMArqEjjnWR3xLfM4Kf"  "(Pair (Pair 1 (Some (Left (Left (Right Unit))))) (Left (Right Unit)) None)"
  {"operation":"{ \"address\": \"DK1NmndDdhkWdWpX7NMArqEjjnWR3xLfM4Kf\",\n  \"argument\":\n    [ \"Pair\",\n      [ [ \"Pair\",\n          [ [ \"Int\", \"1\" ],\n            [ \"Option\",\n              [ \"Some\",\n                [ \"Union\",\n                  [ \"Left\",\n                    [ \"Union\",\n                      [ \"Left\", [ \"Union\", [ \"Right\", [ \"Unit\" ] ] ] ] ] ] ] ] ] ] ],\n        [ \"Pair\",\n          [ [ \"Union\", [ \"Left\", [ \"Union\", [ \"Right\", [ \"Unit\" ] ] ] ] ],\n            [ \"Option\", [ \"None\", {} ] ] ] ] ] ] }","tickets":[]}
