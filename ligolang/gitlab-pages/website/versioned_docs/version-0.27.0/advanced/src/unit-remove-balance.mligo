(*PART 0*)
let under_test : string option = Some "./gitlab-pages/docs/advanced/src/remove-balance.mligo"
let _u = Test.reset_state 5n ([] : nat list)

(*PART 1*)
let bs_addr (i:int) : michelson_program =
  Test.compile_value (Test.nth_bootstrap_account i)

let balances : michelson_program =
  Test.compile_expression_subst under_test
    [%cameligo ({| Map.literal [ (( $a1 : address) , 10tz ) ; (( $a2 : address) , 100tz ) ; (( $a3 : address) , 1000tz ) ]|} : ligo_program)]
    [ ("a1", bs_addr 1) ; ("a2", bs_addr 2) ; ("a3", bs_addr 3) ]


(*PART 2*)
let to_tez (i:nat) : michelson_program =
  Test.compile_expression_subst (None: string option)
    [%cameligo ({| $i * 1tez |} : ligo_program)]
    [("i", Test.compile_value (i) )]

let test =
  List.iter
    (fun ((threshold , expected_size) : nat * nat) ->
      let expected_size = Test.compile_value expected_size in
      let size = 
        Test.compile_expression_subst under_test
          [%cameligo ({| Map.size (balances_under $b $threshold) |} : ligo_program)]
          [ ("b", balances) ; ("threshold", to_tez threshold)]
      in
      let () = Test.log ("expected", expected_size) in
      let () = Test.log ("actual",size) in
      assert (Test.michelson_equal size expected_size)
    )
    [(15n,2n);(130n,1n);(1200n,0n)]