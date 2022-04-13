open Core_bench
open Deku_tests
open Tezos_rpc_tests

(* This file only contains manual tests, to run them you need to have a
   Tezos node running. You may also want to change the node_uri and/or the secret key below. *)

let bench_fetch_block_operations =
  Bench.Test.create ~name:"fetch block operations" (fun () ->
      let _ = fetch_block_operations ~block_hash:None in
      ())

let bench_fetch_block_header =
  Bench.Test.create ~name:"fetch block header" (fun () ->
      let _ = fetch_block_header ~block_hash:None in
      ())

let bench_fetch_constants =
  Bench.Test.create ~name:"fetch constants" (fun () ->
      let _ = fetch_constants ~block_hash:None in
      ())

let tests =
  [
    bench_fetch_block_operations; bench_fetch_block_header; bench_fetch_constants;
  ]

let command = Bench.make_command tests
