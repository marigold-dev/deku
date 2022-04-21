open Deku_tests
open Ledger_tests

let profile_create () =
  ignore (make_ticket ());
  ignore (make_address ());
  ignore (make_tezos_address ())

let profile_deposit () =
  ignore (setup_one_deposit ());
  ignore (setup_four_deposits ())

let profile_transfer () =
  let _ = test_transfer () in
  let _ = test_transfers_4 () in
  ()

let profile_withdraw () =
  let _ = test_withdraw_1 () in
  let _ = test_withdraw_4 () in
  ()

let profile_ledger () =
  profile_create ();
  profile_deposit ();
  profile_transfer ();
  profile_withdraw ()
