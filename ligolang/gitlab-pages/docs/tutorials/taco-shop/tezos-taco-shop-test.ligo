#include "tezos-taco-shop-smart-contract.ligo"

function assert_string_failure (const res : test_exec_result ; const expected : string) : unit is
  block {
  const expected = Test.eval(expected) ;
  } with
    case res of
    | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal (actual, expected))
    | Fail (Other) -> failwith ("contract failed for an unknown reason")
    | Success -> failwith ("bad price check")
    end

const test = block {
  // Originate the contract with a initial storage
  const init_storage =
    map [
      1n -> record [ current_stock = 50n ; max_price = 50tez ] ;
      2n -> record [ current_stock = 20n ; max_price = 75tez ] ; ];
  const (pedro_taco_shop_ta, _code, _size) = Test.originate(buy_taco, init_storage, 0tez) ;
  // Convert typed_address to contract
  const pedro_taco_shop_ctr = Test.to_contract (pedro_taco_shop_ta);
  // Convert contract to address
  const pedro_taco_shop = Tezos.address (pedro_taco_shop_ctr);

  // Test inputs
  const classico_kind = 1n ;
  const unknown_kind = 3n ;

  // Auxiliary function for testing equality in maps
  function eq_in_map (const r : taco_supply; const m : taco_shop_storage; const k : nat) is block {
    var b := case Map.find_opt(k, m) of
    | None -> False
    | Some (v) -> (v.current_stock = r.current_stock) and (v.max_price = r.max_price)
    end
  } with b  ;

  // Purchasing a Taco with 1tez and checking that the stock has been updated
  const ok_case : test_exec_result = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 1tez) ;
  const _unit = case ok_case of
    | Success  -> block {
      const storage = Test.get_storage (pedro_taco_shop_ta) ;
    } with (assert (eq_in_map (record [ current_stock = 49n ; max_price = 50tez ], storage, 1n) and
                    eq_in_map (record [ current_stock = 20n ; max_price = 75tez ], storage, 2n)))
    | Fail (x) -> failwith ("ok test case failed")
  end ;

  // Purchasing an unregistred Taco
  const nok_unknown_kind = Test.transfer_to_contract (pedro_taco_shop_ctr, unknown_kind, 1tez) ;
  const _u = assert_string_failure (nok_unknown_kind, "Unknown kind of taco") ;

  // Attempting to Purchase a Taco with 2tez
  const nok_wrong_price = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 2tez) ;
  const _u = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price") ;
  } with unit

