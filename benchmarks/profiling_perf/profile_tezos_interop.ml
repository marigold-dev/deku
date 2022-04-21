open Deku_tests
open Tezos_interop_tests

let profile_keys () =
  ignore (test_keys_to_string ());
  ignore (test_keys_of_string ());
  ignore (test_key_hashes ());
  ignore (test_secret_to_string ());
  ignore (test_secret_of_string ())

let profile_signature () = ignore (test_verify_signature ())

let profile_contract_hash () =
  ignore (test_contract_hash_to_string ());
  ignore (test_contract_hash_of_string ())

let profile_address () =
  ignore (test_address_to_string ());
  ignore (test_address_of_string ())

let profile_ticket () =
  ignore (test_ticket_to_string ());
  ignore (test_ticket_of_string ())

let profile_operation_hash () = ignore (test_operation_hash_string ())

let profile_forge_transaction () =
  ignore (test_forge_transaction_taquito ());
  ignore (test_forge_transaction_bytes ())

let profile_pack () =
  ignore (test_pack_int_1 ());
  ignore (test_pack_int_minus_1 ());
  ignore (test_pack_bytes_0x ());
  ignore (test_pack_bytes_050001 ());
  ignore (test_pack_pair_int_bytes ());
  ignore (test_pack_pair_pair ());
  ignore (test_pack_list_empty ());
  ignore (test_pack_list_int ());
  ignore (test_pack_list_pair ());
  ignore (test_pack_key ());
  ignore (test_pack_key_hash ());
  ignore (test_pack_address_implicit ());
  ignore (test_pack_address_originated ());
  ignore (test_pack_to_bytes_int ());
  ignore (test_pack_to_bytes_bytes ());
  ignore (test_pack_to_bytes_pair ());
  ignore (test_pack_to_bytes_list ());
  ignore (test_pack_to_bytes_key ());
  ignore (test_pack_to_bytes_key_hash ());
  ignore (test_pack_to_bytes_address_implicit ());
  ignore (test_pack_to_bytes_address_originated ())

let profile_consensus () =
  ignore (test_key_hash_exn ());
  ignore (test_address_exn ());
  ignore (test_hash_validators ());
  ignore (test_hash_block ());
  ignore (test_hash_withdraw_handle ())

let profile_discovery () = ignore (test_discovery ())

let profile_tezos_interop () =
  profile_keys ();
  profile_signature ();
  profile_contract_hash ();
  profile_address ();
  profile_ticket ();
  profile_operation_hash ();
  profile_forge_transaction ();
  profile_pack ();
  profile_consensus ();
  profile_discovery ()
