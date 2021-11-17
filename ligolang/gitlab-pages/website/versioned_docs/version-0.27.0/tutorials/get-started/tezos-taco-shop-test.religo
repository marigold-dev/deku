#include "tezos-taco-shop-smart-contract.religo"

let assert_string_failure = ((res,expected) : (test_exec_result, string)) : unit => {
  let expected = Test.eval (expected) ;
  switch (res) {
  | Fail (Rejected (actual,_)) => assert (Test.michelson_equal (actual, expected))
  | Fail (Other) => failwith ("contract failed for an unknown reason")
  | Success => failwith ("bad price check")
  }
} ;

let test =
  /* originate the contract with a initial storage */
  let init_storage = Map.literal ([
      (1n, { current_stock : 50n , max_price : 50tez }) ,
      (2n, { current_stock : 20n , max_price : 75tez }) , ]) ;
  let (pedro_taco_shop_ta, _code, _size) = Test.originate (buy_taco, init_storage, 0tez) ;
  /* Convert typed_address to contract */
  let pedro_taco_shop_ctr = Test.to_contract (pedro_taco_shop_ta);
  /* Convert contract to address */
  let pedro_taco_shop = Tezos.address (pedro_taco_shop_ctr);

  /* Test inputs */
  let classico_kind = 1n ;
  let unknown_kind = 3n ;

  /* Auxiliary function for testing equality in maps */
  let eq_in_map = ((r, m, k) : (taco_supply, taco_shop_storage, nat)) : bool =>
    switch (Map.find_opt (k, m)) {
    | None => false
    | Some (v) => v.current_stock == r.current_stock && v.max_price == r.max_price
    };

  /* Purchasing a Taco with 1tez and checking that the stock has been updated */
  let ok_case : test_exec_result = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 1tez) ;
  let _u = switch (ok_case) {
    | Success  =>
      let storage = Test.get_storage (pedro_taco_shop_ta) ;
      assert (eq_in_map({ current_stock : 49n , max_price : 50tez }, storage, 1n) &&
              eq_in_map({ current_stock : 20n , max_price : 75tez }, storage, 2n))
    | Fail (x) => failwith ("ok test case failed")
  } ;

  /* Purchasing an unregistred Taco */
  let nok_unknown_kind = Test.transfer_to_contract (pedro_taco_shop_ctr, unknown_kind, 1tez) ;
  let _u = assert_string_failure (nok_unknown_kind, "Unknown kind of taco") ;

  /* Attempting to Purchase a Taco with 2tez */
  let nok_wrong_price = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 2tez) ;
  let _u = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price") ;
  ()
