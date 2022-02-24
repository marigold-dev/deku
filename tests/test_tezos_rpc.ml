open Helpers
open Crypto
open Tezos
open Tezos_rpc

let failwith_err err =
  let failwith s = Format.kasprintf failwith s in
  match err with
  | Tezos_rpc.Error.Json_error _ -> failwith "Json_error"
  | Piaf_body err -> failwith "Piaf_body(%a)" Piaf.Error.pp_hum err
  | Piaf_request err -> failwith "Piaf_request(%a)" Piaf.Error.pp_hum err
  | Response_of_yojson err -> failwith "Response_of_yojson(%s)" err

let node_uri = Uri.of_string "http://localhost:8732"
let secret =
  Secret.of_string "edsk4RbgwutwsEdVNuJsE5JDsxeJ6qFcG8F5rKFGnj5finT6FV46sd"
  |> Option.get
let key = Key.of_secret secret
let key_hash = Key_hash.of_key key

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

let tests =
  [ (* fetch_block_operations ~block_hash:None; *)
    (* listen_to_chain_heads; *)
    (* listen_to_blocks; *) ]

let () = Lwt_list.iter_s (fun test -> test ()) tests |> Lwt_main.run
