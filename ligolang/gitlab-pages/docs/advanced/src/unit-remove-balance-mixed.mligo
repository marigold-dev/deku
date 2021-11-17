(*PART 0*)
#include "remove-balance.mligo"
let _u = Test.reset_state 5n ([] : nat list)

(*PART 1*)
let balances : balances =
  let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
  Map.literal [ (a1 , 10tz ) ; (a2, 100tz ) ; (a3, 1000tz ) ]

(*PART 2*)
let test =
  List.iter
    (fun ((threshold , expected_size) : tez * nat) ->
      let tester (balances, threshold : balances * tez) = Map.size (balances_under balances threshold) in
      let size = Test.run tester (balances, threshold) in
      let expected_size = Test.eval expected_size in
      let () = Test.log ("expected", expected_size) in
      let () = Test.log ("actual",size) in
      assert (Test.michelson_equal size expected_size)
    )
    [(15tez,2n);(130tez,1n);(1200tez,0n)]
