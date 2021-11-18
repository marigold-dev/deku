let filename = "gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.mligo"

let assert_string_failure (res : test_exec_result) (expected : string) : unit =
  let expected = Test.compile_value expected in
  match res with
  | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal actual expected)
  | Fail (Other) -> failwith "contract failed for an unknown reason"
  | Success -> failwith "bad price check"

let test =
  (* originate the contract with a initial storage *)
  let init_storage = Test.compile_expression (Some filename)
    [%cameligo ({| Map.literal [
      (1n, { current_stock = 50n ; max_price = 50tez }) ;
      (2n, { current_stock = 20n ; max_price = 75tez }) ; ]
    |} : ligo_program) ]
  in
  let (pedro_taco_shop, _code, _size) = Test.originate_from_file filename "buy_taco" init_storage 0tez in
  (* compile test inputs *)
  let classico_kind = Test.compile_value 1n in
  let unknown_kind = Test.compile_value 3n in

  (* Purchasing a Taco with 1tez and checking that the stock has been updated *)
  let ok_case : test_exec_result = Test.transfer pedro_taco_shop classico_kind 1tez in
  let () = match ok_case with
    | Success  ->
      let storage = Test.get_storage_of_address pedro_taco_shop in
      let expected = Test.compile_expression (Some filename)
        [%cameligo ({| Map.literal [
          (1n, { current_stock = 49n ; max_price = 50tez }) ;
          (2n, { current_stock = 20n ; max_price = 75tez }) ; ]
        |} : ligo_program) ]
      in
      assert (Test.michelson_equal expected storage)
    | Fail x -> failwith ("ok test case failed")
  in

  (* Purchasing an unregistred Taco *)
  let nok_unknown_kind = Test.transfer pedro_taco_shop unknown_kind 1tez in
  let () = assert_string_failure nok_unknown_kind "Unknown kind of taco" in

  (* Attempting to Purchase a Taco with 2tez *)
  let nok_wrong_price = Test.transfer pedro_taco_shop classico_kind 2tez in
  let () = assert_string_failure nok_wrong_price "Sorry, the taco you are trying to purchase has a different price" in
  ()
