open Deku_stdlib
open Deku_indexer
open Handlers
include Node

let error_to_response error =
  let status = Api_error.to_http_code error |> Dream.int_to_status in
  let body = Api_error.yojson_of_t error |> Yojson.Safe.to_string in
  Dream.json ~status body

let make_handler (node : t) (indexer : Indexer.t) (constants : Api_constants.t)
    (module Handler : HANDLER) =
  let handler request =
    let%await input = Handler.input_from_request request in
    match input with
    | Error error -> error_to_response error
    | Ok input -> (
        let%await response = Handler.handle ~node ~indexer ~constants input in
        match response with
        | Ok response ->
            let body =
              Handler.yojson_of_response response |> Yojson.Safe.to_string
            in
            Dream.json ~status:`OK body
        | Error error -> error_to_response error)
  in
  let method_not_allowed_handler _ =
    Api_error.method_not_allowed Handler.path Handler.meth |> error_to_response
  in
  let route =
    match Handler.meth with
    | `POST -> Dream.post Handler.path handler
    | `GET -> Dream.get Handler.path handler
  in
  [ route; Dream.any Handler.path method_not_allowed_handler ]

let cors_middleware handler req =
  let%await response = handler req in
  Dream.add_header response "Access-Control-Allow-Origin" "*";
  Dream.add_header response "Access-Control-Allow-Headers" "*";
  Dream.add_header response "Allow" "*";
  Lwt.return response

let no_cache_middleware handler req =
  let%await response = handler req in
  Dream.add_header response "Cache-Control" "max-age=0, no-cache, no-store";
  Lwt.return response

let make_routes node indexer constants =
  cors_middleware @@ no_cache_middleware
  @@ Dream.router
       [
         Dream.scope "/api/v1/" []
           (List.flatten
              [
                make_handler node indexer constants (module Get_genesis);
                make_handler node indexer constants (module Get_head);
                make_handler node indexer constants (module Get_level);
                make_handler node indexer constants (module Get_chain_info);
                make_handler node indexer constants
                  (module Get_block_by_level_or_hash);
                make_handler node indexer constants
                  (module Helpers_operation_message);
                make_handler node indexer constants
                  (module Helpers_hash_operation);
                make_handler node indexer constants (module Post_operation);
                make_handler node indexer constants (module Get_balance);
                make_handler node indexer constants (module Get_proof);
                make_handler node indexer constants (module Get_vm_state);
              ]);
       ]
