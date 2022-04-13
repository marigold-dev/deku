open Core_bench
open Deku_tests
open Tezos_interop_tests

(*************************************************************************)
(* public key *)

(* NOTE TODO: Maybe I need to put the test of equality here?,
   note about the test does not check as in test when given a wrong value,
   it still pass. *)

let bench_keys_to_string =
  Bench.Test.create ~name:"key: to_string" (fun () ->
      let _ = test_keys_to_string () in
      ())

let bench_keys_of_string =
  Bench.Test.create ~name:"key: of_string" (fun () ->
      let _ = test_keys_of_string () in
      ())

let bench_key_hashes =
  Bench.Test.create ~name:"key: hash" (fun () ->
      let _ = test_key_hashes () in
      ())

(*************************************************************************)
(* secret key *)

let bench_secret_to_string =
  Bench.Test.create ~name:"secret: to_string" (fun () ->
      let _ = test_secret_to_string () in
      ())

let bench_secret_of_string =
  Bench.Test.create ~name:"secret: of_string" (fun () ->
      let _ = test_secret_of_string () in
      ())

(*************************************************************************)
(* verify signature *)

let bench_verify_signature =
  Bench.Test.create ~name:"verify signature" (fun () ->
      let _ = test_verify_signature () in
      ())

(*************************************************************************)
(* contract hash *)

let bench_contract_hash_to_string =
  Bench.Test.create ~name:"contract hash: to_string" (fun () ->
      let _ = test_contract_hash_to_string () in
      ())

let bench_contract_hash_of_string =
  Bench.Test.create ~name:"contract hash: of_string" (fun () ->
      let _ = test_contract_hash_of_string () in
      ())

(*************************************************************************)
(* address *)

let bench_address_to_string =
  Bench.Test.create ~name:"address: to_string" (fun () ->
      let _ = test_address_to_string () in
      ())

let bench_address_of_string =
  Bench.Test.create ~name:"address: of_string" (fun () ->
      let _ = test_address_of_string () in
      ())

(*************************************************************************)
(* ticket *)

let bench_ticket_to_string =
  Bench.Test.create ~name:"ticket: to_string" (fun () ->
      let _ = test_ticket_to_string () in
      ())

let bench_ticket_of_string =
  Bench.Test.create ~name:"ticket: of_string" (fun () ->
      let _ = test_ticket_of_string () in
      ())

(*************************************************************************)
(* operation_hash *)

let bench_operation_hash_string =
  Bench.Test.create ~name:"operation hash: to_string, of_string" (fun () ->
      let _ = test_operation_hash_string () in
      ())

(*************************************************************************)
(* operation forging *)

let bench_forge_transaction_taquito =
  Bench.Test.create ~name:"forge transaction taquito" (fun () ->
      let _ = test_forge_transaction_taquito () in
      ())

let bench_forge_transaction_bytes =
  Bench.Test.create ~name:"forge transaction bytes" (fun () ->
      let _ = test_forge_transaction_bytes () in
      ())

(*************************************************************************)
(* pack *)

let bench_pack_int_1 =
  Bench.Test.create ~name:"pack: int 1" (fun () ->
      let _ = test_pack_int_1 () in
      ())

let bench_pack_int_minus_1 =
  Bench.Test.create ~name:"pack: int -1" (fun () ->
      let _ = test_pack_int_minus_1 () in
      ())

let bench_pack_bytes_0x =
  Bench.Test.create ~name:"pack: bytes 0x" (fun () ->
      let _ = test_pack_bytes_0x () in
      ())

let bench_pack_bytes_050001 =
  Bench.Test.create ~name:"pack: bytes 050001" (fun () ->
      let _ = test_pack_bytes_050001 () in
      ())

let bench_pack_pair_int_bytes =
  Bench.Test.create ~name:"pack: pair: (1, 0x)" (fun () ->
      let _ = test_pack_pair_int_bytes () in
      ())

let bench_pack_pair_pair =
  Bench.Test.create ~name:"pack: (1, (0xAA, -1))" (fun () ->
      let _ = test_pack_pair_pair () in
      ())

let bench_pack_list_empty =
  Bench.Test.create ~name:"pack: list empty" (fun () ->
      let _ = test_pack_list_empty () in
      ())

let bench_pack_list_int =
  Bench.Test.create ~name:"pack: list int 1" (fun () ->
      let _ = test_pack_list_int () in
      ())

let bench_pack_list_pair =
  Bench.Test.create ~name:"pack: list pair" (fun () ->
      let _ = test_pack_list_pair () in
      ())

let bench_pack_key =
  Bench.Test.create ~name:"pack: key" (fun () ->
      let _ = test_pack_key () in
      ())

let bench_pack_key_hash =
  Bench.Test.create ~name:"pack: key_hash" (fun () ->
      let _ = test_pack_key_hash () in
      ())

let bench_pack_address_implicit =
  Bench.Test.create ~name:"pack: address implicit" (fun () ->
      let _ = test_pack_address_implicit () in
      ())

let bench_pack_address_originated =
  Bench.Test.create ~name:"pack: address originated" (fun () ->
      let _ = test_pack_address_originated () in
      ())

let bench_pack_to_bytes_int =
  Bench.Test.create ~name:"pack: to_bytes int" (fun () ->
      let _ = test_pack_to_bytes_int () in
      ())

let bench_pack_to_bytes_bytes =
  Bench.Test.create ~name:"pack: to_bytes bytes" (fun () ->
      let _ = test_pack_to_bytes_bytes () in
      ())

let bench_pack_to_bytes_pair =
  Bench.Test.create ~name:"pack: to_bytes pair" (fun () ->
      let _ = test_pack_to_bytes_pair () in
      ())

let bench_pack_to_bytes_list =
  Bench.Test.create ~name:"pack: to_bytes list" (fun () ->
      let _ = test_pack_to_bytes_list () in
      ())

let bench_pack_to_bytes_key =
  Bench.Test.create ~name:"pack: to_bytes key" (fun () ->
      let _ = test_pack_to_bytes_key () in
      ())

let bench_pack_to_bytes_key_hash =
  Bench.Test.create ~name:"pack: to_bytes key_hash" (fun () ->
      let _ = test_pack_to_bytes_key_hash () in
      ())

let bench_pack_to_bytes_address_implicit =
  Bench.Test.create ~name:"pack: to_bytes address implicit" (fun () ->
      let _ = test_pack_to_bytes_address_implicit () in
      ())

let bench_pack_to_bytes_address_originated =
  Bench.Test.create ~name:"pack: to_bytes address originated" (fun () ->
      let _ = test_pack_to_bytes_address_originated () in
      ())

(*************************************************************************)
(* consensus *)

let bench_key_hash_exn =
  Bench.Test.create ~name:"consensus: key_hash_exn" (fun () ->
      let _ = test_key_hash_exn () in
      ())

let bench_address_exn =
  Bench.Test.create ~name:"consensus: address_exn" (fun () ->
      let _ = test_address_exn () in
      ())

let bench_hash_validators =
  Bench.Test.create ~name:"consensus: hash_validators" (fun () ->
      let _ = test_hash_validators () in
      ())

let bench_hash_block =
  Bench.Test.create ~name:"consensus: hash_block" (fun () ->
      let _ = test_hash_block () in
      ())

let bench_hash_withdraw_handle =
  Bench.Test.create ~name:"consensus: hash_withdraw_handle" (fun () ->
      let _ = test_hash_withdraw_handle () in
      ())

(*************************************************************************)
(* discovery *)

let bench_discovery =
  Bench.Test.create ~name:"discovery" (fun () ->
      let _ = test_discovery () in
      ())

(*************************************************************************)

let tests =
  [
    bench_keys_to_string;
    bench_keys_of_string;
    bench_key_hashes;
    bench_secret_to_string;
    bench_secret_of_string;
    bench_verify_signature;
    bench_contract_hash_to_string;
    bench_contract_hash_of_string;
    bench_address_to_string;
    bench_address_of_string;
    bench_ticket_to_string;
    bench_ticket_of_string;
    bench_address_of_string;
    bench_operation_hash_string;
    bench_forge_transaction_taquito;
    bench_forge_transaction_bytes;
    bench_pack_int_1;
    bench_pack_int_minus_1;
    bench_pack_bytes_0x;
    bench_pack_bytes_050001;
    bench_pack_pair_int_bytes;
    bench_pack_pair_pair;
    bench_pack_list_empty;
    bench_pack_list_int;
    bench_pack_list_pair;
    bench_pack_key;
    bench_pack_key_hash;
    bench_pack_address_implicit;
    bench_pack_address_originated;
    bench_pack_to_bytes_int;
    bench_pack_to_bytes_bytes;
    bench_pack_to_bytes_list;
    bench_pack_to_bytes_pair;
    bench_pack_to_bytes_key;
    bench_pack_to_bytes_key_hash;
    bench_pack_to_bytes_address_implicit;
    bench_pack_to_bytes_address_originated;
    bench_address_exn;
    bench_key_hash_exn;
    bench_hash_validators;
    bench_hash_block;
    bench_hash_withdraw_handle;
    bench_discovery;
  ]

let command = Bench.make_command tests
