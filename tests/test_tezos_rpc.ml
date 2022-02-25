open Helpers
open Crypto
open Tezos
open Tezos_rpc

(* This file only contains manual tests, to run them you need to have a
   Tezos node running and uncomment the tests on the tests list below.

   You may also want to change the node_uri and/or the secret key below. *)

let node_uri = Uri.of_string "http://localhost:8732"
let secret =
  Secret.of_string "edsk4RbgwutwsEdVNuJsE5JDsxeJ6qFcG8F5rKFGnj5finT6FV46sd"
  |> Option.get
let key = Key.of_secret secret
let key_hash = Key_hash.of_key key

let failwith_err err =
  let failwith s = Format.kasprintf failwith s in
  match err with
  | Tezos_rpc.Error.Json_error _ -> failwith "Json_error"
  | Piaf_body err -> failwith "Piaf_body(%a)" Piaf.Error.pp_hum err
  | Piaf_request err -> failwith "Piaf_request(%a)" Piaf.Error.pp_hum err
  | Response_of_yojson err -> failwith "Response_of_yojson(%s)" err

let fetch_block_operations ~block_hash () =
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

(* WARNING: This will never stop.
   It also doesn't show in the terminal, so use dune exec *)
let listen_to_chain_heads () =
  let%await result =
    Tezos_rpc.Listen_to_chain_heads.execute ~node_uri ~chain:None in
  match result with
  | Ok stream ->
    Lwt_stream.iter
      (fun header ->
        let hash = header.Tezos_rpc.Block_header.hash in
        Format.eprintf "header.hash: %s\n%!" (Block_hash.to_string hash))
      stream
  | Error err -> failwith_err err

(* WARNING: This will never stop.
   It also doesn't show in the terminal, so use dune exec *)
let listen_to_blocks () =
  let%await result = Tezos_rpc.Listen_to_blocks.execute ~node_uri in
  match result with
  | Ok stream ->
    Lwt_stream.iter
      (fun header ->
        let hash = header.Tezos_rpc.Block_header.hash in
        Format.printf "header.hash: %s\n%!" (Block_hash.to_string hash))
      stream
  | Error err -> failwith_err err

let fetch_block_header ~block_hash () =
  let open Tezos_rpc.Fetch_block_header in
  let%await result = execute ~node_uri ~chain:None ~block_hash in
  (match result with
  | Ok header ->
    Format.printf "header.hash: %s, header.level: %ld\n%!"
      (Block_hash.to_string header.hash)
      header.level
  | Error err -> failwith_err err);
  Lwt.return_unit

let fetch_constants ~block_hash () =
  let open Tezos_rpc.Fetch_constants in
  let%await result = execute ~node_uri ~chain:None ~block_hash in
  (match result with
  | Ok constants ->
    Format.printf "hard_storage_limit_per_operation: %a\n%!" Z.pp_print
      constants.hard_storage_limit_per_operation
  | Error err -> failwith_err err);
  Lwt.return_unit

(* to run a test, just uncomment it *)
let tests =
  [ (* fetch_block_operations ~block_hash:None; *)
    (* listen_to_chain_heads; *)
    (* listen_to_blocks; *)
    (* fetch_block_header ~block_hash:None; *)
    (* fetch_constants ~block_hash:None; *) ]

let () = Lwt_list.iter_s (fun test -> test ()) tests |> Lwt_main.run
