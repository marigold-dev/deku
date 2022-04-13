open Bench_utils
open Deku_tests
open Tezos_interop_tests

(*************************************************************************)
(* public key *)

let bench_keys () =
  let list_bench =
    [
      ("keys: to_string", (fun () -> ignore (test_keys_to_string ())), ());
      ("keys: of_string", (fun () -> ignore (test_keys_of_string ())), ());
      ("keys: hash", (fun () -> ignore (test_key_hashes ())), ());
      ( "secret keys: to_string",
        (fun () -> ignore (test_secret_to_string ())),
        () );
      ( "secret keys: of_string",
        (fun () -> ignore (test_secret_of_string ())),
        () );
    ] in
  bench_throughput_latency "Benchmark keys" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*************************************************************************)
(* verify signature *)

let bench_signature () =
  let list_bench =
    [("verify signature", (fun () -> ignore (test_verify_signature ())), ())]
  in
  bench_throughput_latency "Benchmark verify signature" ~repeat:5 ~time:10
    list_bench ~latency:20_000L

(*************************************************************************)
(* contract hash *)

let bench_contract_hash () =
  let list_bench =
    [
      ( "contract_hash: to_string",
        (fun () -> ignore (test_contract_hash_to_string ())),
        () );
      ( "contract_hash: of_string",
        (fun () -> ignore (test_contract_hash_of_string ())),
        () );
    ] in
  bench_throughput_latency "Benchmark contract hash" ~repeat:5 ~time:10
    list_bench ~latency:20_000L

(*************************************************************************)
(* address *)

let bench_address () =
  let list_bench =
    [
      ("address: to_string", (fun () -> ignore (test_address_to_string ())), ());
      ("address: of_string", (fun () -> ignore (test_address_of_string ())), ());
    ] in
  bench_throughput_latency "Benchmark address" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*************************************************************************)
(* ticket *)

let bench_ticket () =
  let list_bench =
    [
      ("ticket: to_string", (fun () -> ignore (test_ticket_to_string ())), ());
      ("ticket: of_string", (fun () -> ignore (test_ticket_of_string ())), ());
    ] in
  bench_throughput_latency "Benchmark ticket" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*************************************************************************)
(* operation_hash *)

let bench_operation_hash () =
  let list_bench =
    [("operation hash", (fun () -> ignore (test_operation_hash_string ())), ())]
  in
  bench_throughput_latency "Benchmark operation hash" ~repeat:5 ~time:10
    list_bench ~latency:20_000L

(*************************************************************************)
(* operation forging *)

let bench_forge_transaction () =
  let list_bench =
    [
      ( "forge transaction taquito",
        (fun () -> ignore (test_forge_transaction_taquito ())),
        () );
      ( "forge transaction bytes",
        (fun () -> ignore (test_forge_transaction_bytes ())),
        () );
    ] in
  bench_throughput_latency "Benchmark forge transaction " ~repeat:5 ~time:10
    list_bench ~latency:20_000L

(*************************************************************************)
(* pack *)

let bench_pack () =
  let list_bench =
    [
      ("pack: int 1", (fun () -> ignore (test_pack_int_1 ())), ());
      ("pack: int -1", (fun () -> ignore (test_pack_int_minus_1 ())), ());
      ("pack: bytes 0x", (fun () -> ignore (test_pack_bytes_0x ())), ());
      ("pack: bytes 050001", (fun () -> ignore (test_pack_bytes_050001 ())), ());
      ( "pack: pair (1, 0x)",
        (fun () -> ignore (test_pack_pair_int_bytes ())),
        () );
      ("pack: (1, (0xAA, -1))", (fun () -> ignore (test_pack_pair_pair ())), ());
      ("pack: list empty", (fun () -> ignore (test_pack_list_empty ())), ());
      ("pack: list int 1", (fun () -> ignore (test_pack_list_int ())), ());
      ("pack: list pair", (fun () -> ignore (test_pack_list_pair ())), ());
      ("pack: key", (fun () -> ignore (test_pack_key ())), ());
      ("pack: key hash", (fun () -> ignore (test_pack_key_hash ())), ());
      ( "pack: address implicit",
        (fun () -> ignore (test_pack_address_implicit ())),
        () );
      ( "pack: address originated",
        (fun () -> ignore (test_pack_address_originated ())),
        () );
      ("pack: to_bytes int", (fun () -> ignore (test_pack_to_bytes_int ())), ());
      ( "pack: to_bytes bytes",
        (fun () -> ignore (test_pack_to_bytes_bytes ())),
        () );
      ( "pack: to_bytes pair",
        (fun () -> ignore (test_pack_to_bytes_pair ())),
        () );
      ( "pack: to_bytes list",
        (fun () -> ignore (test_pack_to_bytes_list ())),
        () );
      ("pack: to_bytes key", (fun () -> ignore (test_pack_to_bytes_key ())), ());
      ( "pack: to_bytes key_hash",
        (fun () -> ignore (test_pack_to_bytes_key_hash ())),
        () );
      ( "pack: to_bytes address_implicit",
        (fun () -> ignore (test_pack_to_bytes_address_implicit ())),
        () );
      ( "pack: to_bytes address_originated",
        (fun () -> ignore (test_pack_to_bytes_address_originated ())),
        () );
    ] in

  bench_throughput_latency "Benchmark pack " ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*************************************************************************)
(* consensus *)

let bench_consensus () =
  let list_bench =
    [
      ("consensus: key_hash_exn", (fun () -> ignore (test_key_hash_exn ())), ());
      ("consensus: address_exn", (fun () -> ignore (test_address_exn ())), ());
      ( "consensus: hash_validators",
        (fun () -> ignore (test_hash_validators ())),
        () );
      ("consensus: hash_block", (fun () -> ignore (test_hash_block ())), ());
      ( "consensus: hash_withdraw_handle",
        (fun () -> ignore (test_hash_withdraw_handle ())),
        () );
    ] in
  bench_throughput_latency "Benchmark forge transaction " ~repeat:5 ~time:10
    list_bench ~latency:20_000L

(*************************************************************************)
(* discovery *)

let bench_discovery () =
  let list_bench = [("discovery", (fun () -> ignore (test_discovery ())), ())] in
  bench_throughput_latency "Benchmark forge transaction " ~repeat:5 ~time:10
    list_bench ~latency:20_000L

let benchmark_tezos_interop () =
  bench_keys ();
  bench_signature ();
  bench_contract_hash ();
  bench_address ();
  bench_ticket ();
  bench_operation_hash ();
  bench_forge_transaction ();
  bench_pack ();
  bench_consensus ();
  bench_discovery ()
