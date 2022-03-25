open Core_bench
open Tezos_rpc
open Helpers
open Tezos

(* This file only contains manual tests, to run them you need to have a
   Tezos node running and uncomment the tests on the tests list below.
   You may also want to change the node_uri and/or the secret key below. *)

let node_uri = Uri.of_string "http://localhost:8732"

let failwith_err err =
  let failwith s = Format.kasprintf failwith s in
  match err with
  | Tezos_rpc.Error.Json_error _ -> failwith "Json error"
  | Piaf_body err -> failwith "Piaf body(%a)" Piaf.Error.pp_hum err
  | Piaf_request err -> failwith "Piaf_request(%a)" Piaf.Error.pp_hum err
  | Response_of_yojson err -> failwith "Response of yojson(%s)" err

let fetch_block_operations ~block_hash =
  let open Fetch_block_operations in
  let%await result = execute ~node_uri ~chain:None ~block_hash in
  (match result with
  | Ok operations ->
    List.iter
      (fun operation ->
        Format.printf "operation.hash: %s\n%!"
          (Operation_hash.to_string operation.hash))
      operations
  | Error err -> failwith_err err);
  Lwt.return_unit

let bench_fetch_block_operations =
  Bench.Test.create ~name:"fetch block operations" (fun () ->
      let _ = fetch_block_operations ~block_hash:None in
      ())

let fetch_block_header ~block_hash =
  let open Tezos_rpc.Fetch_block_header in
  let%await result = execute ~node_uri ~chain:None ~block_hash in
  (match result with
  | Ok header ->
    Format.printf "header_hash: %s, header_level:%ld\n%!"
      (Block_hash.to_string header.hash)
      header.level
  | Error err -> failwith_err err);
  Lwt.return_unit

let bench_fetch_block_header =
  Bench.Test.create ~name:"fetch block header" (fun () ->
      let _ = fetch_block_header ~block_hash:None in
      ())

let fetch_constants ~block_hash =
  let open Tezos_rpc.Fetch_constants in
  let%await result = execute ~node_uri ~chain:None ~block_hash in
  (match result with
  | Ok constants ->
    Format.printf "hard_storage_limit_per_operation: %a\n%!" Z.pp_print
      constants.hard_storage_limit_per_operation
  | Error err -> failwith_err err);
  Lwt.return_unit

let bench_fetch_constants =
  Bench.Test.create ~name:"fetch constants" (fun () ->
      let _ = fetch_constants ~block_hash:None in
      ())

let tests =
  [
    bench_fetch_block_operations; bench_fetch_block_header; bench_fetch_constants;
  ]

let command = Bench.make_command tests