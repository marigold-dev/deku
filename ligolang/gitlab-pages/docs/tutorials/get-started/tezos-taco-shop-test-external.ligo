const filename = "gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.ligo"

function assert_string_failure (const res : test_exec_result ; const expected : string) : unit is
  block {
  const expected = Test.compile_value(expected) ;
  } with
    case res of
    | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal (actual, expected))
    | Fail (Other) -> failwith ("contract failed for an unknown reason")
    | Success -> failwith ("bad price check")
    end

const test = block {
  // originate the contract with a initial storage
  const init_storage = Test.compile_expression (Some (filename),
    [%pascaligo ({| map [
      1n -> record [ current_stock = 50n ; max_price = 50tez ] ;
      2n -> record [ current_stock = 20n ; max_price = 75tez ] ; ]
    |} : ligo_program) ] ) ;
  const (pedro_taco_shop, _code, _size) = Test.originate_from_file (filename, "buy_taco", init_storage, 0tez) ;
  // compile test inputs
  const classico_kind = Test.compile_value (1n) ;
  const unknown_kind = Test.compile_value (3n) ;

  // Purchasing a Taco with 1tez and checking that the stock has been updated
  const ok_case : test_exec_result = Test.transfer (pedro_taco_shop, classico_kind, 1tez) ;
  const _unit = case ok_case of
    | Success  -> block {
      const storage = Test.get_storage_of_address (pedro_taco_shop) ;
      const expected = Test.compile_expression (Some (filename),
        [%pascaligo ({| map [
          1n -> record [ current_stock = 49n ; max_price = 50tez ] ;
          2n -> record [ current_stock = 20n ; max_price = 75tez ] ; ]
        |} : ligo_program) ] ) ;
    } with (assert (Test.michelson_equal (expected, storage)))
    | Fail (x) -> failwith ("ok test case failed")
  end ;

  // Purchasing an unregistred Taco
  const nok_unknown_kind = Test.transfer (pedro_taco_shop, unknown_kind, 1tez) ;
  const _u = assert_string_failure (nok_unknown_kind, "Unknown kind of taco") ;

  // Attempting to Purchase a Taco with 2tez
  const nok_wrong_price = Test.transfer (pedro_taco_shop, classico_kind, 2tez) ;
  const _u = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price") ;
  } with unit
