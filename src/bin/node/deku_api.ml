open Deku_stdlib
open Deku_indexer
open Handlers
include Node

let error_to_response error =
  let status = Api_error.to_http_code error |> Dream.int_to_status in
  let body = Api_error.yojson_of_t error |> Yojson.Safe.to_string in
  Dream.json ~status body

let method_not_allowed_handler path meth _ =
  Api_error.method_not_allowed path meth |> error_to_response

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
  let method_not_allowed_handler req =
    method_not_allowed_handler Handler.path Handler.meth req
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

(* Websockets *)
let counter = ref 0

module Websockets = Stdlib.Map.Make (Int)

let websockets = ref Websockets.empty

let ws_block_monitor =
  let handler _ =
    Dream.websocket ~close:false (fun websocket ->
        let id = !counter + 1 in
        counter := id;
        websockets := Websockets.add id websocket !websockets;
        let rec loop () =
          let%await msg = Dream.receive websocket in
          match msg with
          | Some _msg -> loop ()
          | None ->
              let%await () = Dream.close_websocket websocket in
              websockets := Websockets.remove id !websockets;
              Lwt.return_unit
        in
        loop ())
  in
  let path = "/chain/blocks/monitor" in
  let meth = `GET in
  let route = Dream.get "/chain/blocks/monitor" handler in
  let method_not_allowed_handler req =
    method_not_allowed_handler path meth req
  in
  [ route; Dream.any path method_not_allowed_handler ]

(* Client just needs to know the hash, level and the list of operations
   If the client wants to know further informations on a block, he can always query chain/blocks/{hash/level}
*)
module BlockDTO = struct
  open Deku_consensus
  open Deku_concepts
  open Deku_protocol

  type block = {
    hash : Block_hash.t;
    level : Level.t;
    operations : Operation_hash.t list;
  }
  [@@deriving yojson_of]

  type t = block

  let of_block (block : Block.t) =
    let (Block.Block { hash; level; payload; _ }) = block in
    (* TODO: deserialize in a parallel pool ?? *)
    let parse_operation operation =
      match
        let json = Yojson.Safe.from_string operation in
        let operation = Operation.t_of_yojson json in
        let (Operation.Operation { hash; _ }) = operation in
        hash
      with
      | hash -> Some hash
      | exception _ -> None
    in
    let operations = payload |> List.filter_map parse_operation in
    { hash; level; operations }
end

let on_block block =
  let block =
    BlockDTO.of_block block |> BlockDTO.yojson_of_block |> Yojson.Safe.to_string
  in
  Lwt_list.iter_p
    (fun (_id, websocket) ->
      let%await () = Dream.send websocket block in
      Lwt.return_unit)
    (!websockets |> Websockets.to_seq |> List.of_seq)

let on_block block = Lwt.async (fun () -> on_block block)

let make_routes node indexer constants =
  cors_middleware @@ no_cache_middleware
  @@ Dream.router
       [
         Dream.scope "/api/v1/" []
           (List.flatten
              [
                ws_block_monitor;
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
                make_handler node indexer constants (module Get_vm_state);
              ]);
       ]